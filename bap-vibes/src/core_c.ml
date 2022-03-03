(***************************************************************************)
(*                                                                         *)
(*  Copyright (C) 2022/2023 The Charles Stark Draper Laboratory, Inc.      *)
(*                                                                         *)
(*  This file is provided under the license found in the LICENSE file in   *)
(*  the top-level directory of this project.                               *)
(*                                                                         *)
(*  This research was developed with funding from the Defense Advanced     *)
(*  Research Projects Agency (DARPA).                                      *)
(*                                                                         *)
(***************************************************************************)

(*******************************************************************
 * This module implements a [Theory.Core] interpretation for [FrontC]
 * function bodies.
 *
 * Roughly speaking, a C function can be interpreted as a Core [unit
 * Theory.eff], which is just a side effectful computation in the
 * language of BAP's final tagless generic representation.
 *
 * The idea is quite straightforward:
 *
 * In any [Theory.Core] instance, there are a series of functions which
 * construct various values of different built-in types. E.g. [set] takes
 * a variable and a "pure" value, and returns an effect (morally the
 * effect of setting that variable to that value).
 *
 * See the documentation of [Theory.Core] for (much) more detail.
 *
 * So we use this language to build the semantics of the various bits of
 * C syntax. The main complications are as follows:
 *
 * - C has complex types and operations over those types. The [PatchC] 
 *   intermediate language has this information for us, and ensures that
 *   the program is explicitly and strictly typed. Later, we apply the
 *   remaining implicit conversion rules when lowering to [Core].
 *   
 * - Since [Core] uses phantom types to be type-safe, we must re-sort all
 *   the arguments and cast them to "unit" (top) type using [resort].
 *
 * - [Core] does not have a construct for function calls (in particular,
 *   function calls with arguments!). Program labels in [Core] can be 
 *   annotated with a predicate that declares that they are at the start
 *   of a subroutine. Then, in the BAP IR theory, these control-flow
 *   constructs are correctly reified to function calls. For arguments
 *   we associate each particular call destination with a set of variables
 *   that were assigned leading up to the call. For this reason, each
 *   label for the destination must be unique.
 *
 * - The width of machine words, and the memory layout are stored in the
 *   [interp_info] type, which can be generated by reading the relevant values
 *   from the [Theory.target] which corresponds to the target architecture.
 *
 * - Jumps to concrete addresses are not possible directly in C. We support
 *   an alternate syntax: [goto L_some_hex_number], which in C syntax is an
 *   ordinary [goto] to a label, but which is handled specially by our
 *   processor.
 *
 * - Handling the ADDROF operator requires us to know the storage
 *   classification of an l-value in C, or to allocate on the stack.
 *   To keep things simple, we favor the former approach by asking the user
 *   to provide this information in the higher vars.
 ***********************************************************************)

open Core_kernel
open Bap_core_theory
open Bap.Std
open KB.Let

module Hvar = Higher_var
module Err = Kb_error

type var_map = unit Theory.var String.Map.t

let arg_vars = KB.Class.property Theory.Program.cls "arg-vars" @@
  KB.Domain.optional "arg-vars-domain" ~equal:(List.equal Var.equal)

let provide_args (e : Theory.label) (args : var list) : unit KB.t =
  KB.provide arg_vars e (Some args)

let collect_args (e : Theory.label) : var list KB.t =
  let+ args = KB.collect arg_vars e in
  Option.value ~default:[] args

(* We need to mark calls as such in the KB, so that the instruction selector
   knows to lower to a call instead of a normal jump. *)
let declare_call (e : Theory.label) : unit KB.t =
  KB.provide Theory.Label.is_subroutine e (Some true)

(* We return false by default, if it is unlabeled *)
let is_call (e : Theory.label) : bool KB.t =
  let+ e = KB.collect Theory.Label.is_subroutine e in
  Option.value ~default:false e

module Eval(CT : Theory.Core) = struct

  module T = Theory
  open KB.Syntax

  (* If `endian` evaluates to `b1` (true), then we are big-endian. *)
  type ('a, 'b) interp_info = {
    tgt       : Theory.target;
    word_sort : 'a T.Bitv.t T.Value.sort;
    byte_sort : 'b T.Bitv.t T.Value.sort;
    mem_var   : ('a, 'b) T.Mem.t T.var;
    endian    : T.Bool.t T.value;
    ret_var   : unit T.var;
    arg_vars  : unit T.var list;
    hvars     : Hvar.t list;
  }

  let make_reg (v : unit T.var) : unit T.var =
    let sort = T.Var.sort v in
    let v = Substituter.mark_reg @@ Var.reify v in
    T.Var.create sort @@ Var.ident v

  let mk_interp_info
      (hvars : Hvar.t list)
      (tgt : T.target) : ('a, 'b) interp_info KB.t =
    let bits = T.Target.bits tgt in
    let word_sort = T.Bitv.define bits in
    let byte_sort = T.Bitv.define (T.Target.byte tgt) in
    let ret_var = T.(Target.reg tgt Role.Register.function_return) in
    let arg_vars = T.(Target.regs tgt ~roles:Role.Register.[function_argument]) in
    let endianness = T.Target.endianness tgt in
    let+ endian =
      if T.Endianness.(endianness = eb) then CT.b1 else CT.b0 in
    (* We assume mem_sort is indexed on word size into bytes, since
       it's the most common. Otherwise we need to query [tgt]
       more... *)
    {
      tgt;
      word_sort = word_sort;
      byte_sort = byte_sort;
      mem_var = T.Target.data tgt;
      endian;
      ret_var = make_reg @@ Option.value_exn ret_var;
      arg_vars = Set.to_list arg_vars |> List.map ~f:make_reg;
      hvars;
    }

  let char_ty = T.Bitv.define 8
  let short_ty = T.Bitv.define 16
  let long_ty = T.Bitv.define 32
  let long_long_ty = T.Bitv.define 64

  let ty_of_base_type
      (info : _ interp_info)
      (c_ty : Patch_c.typ) : _ T.Bitv.t T.Value.sort =
    match c_ty with
    | VOID -> T.Bitv.define 0
    | INT (`r8, _) -> char_ty
    | INT (`r16, _) -> short_ty
    | INT (`r32, _) -> long_ty
    | INT (`r64, _) -> long_long_ty
    | INT _ -> assert false
    | PTR _ | FUN _ -> info.word_sort

  let ty_op_pointer_type
      (info : _ interp_info)
      (c_ty : Patch_c.typ) : _ T.Bitv.t T.Value.sort =
    match c_ty with
    | PTR ty -> ty_of_base_type info ty
    | _ -> info.word_sort

  let resort (sort : 'a T.Value.sort) (v : 'b T.value) : 'a T.value KB.t =
    let error = "Incorrect argument sort!" in
    T.Value.resort (fun _ -> Some sort) v |>
    Option.value_exn ~message:error |>
    KB.return

  type 'a pure = 'a T.pure

  let lift_binop
      (op : 'a T.value KB.t -> 'b T.value KB.t -> 'c T.value KB.t)
      (sort_a : 'a T.Value.sort)
      (sort_b : 'b T.Value.sort)
      (a : unit pure)
      (b : unit pure) : unit pure =
    let* a = a in
    let* b = b in
    let* res = op (resort sort_a a) (resort sort_b b) in
    KB.return @@ T.Value.forget res

  let binop_to_pure
      (info : _ interp_info)
      (op : Patch_c.binop)
      (ty_a : Patch_c.typ)
      (ty_b : Patch_c.typ) : (unit pure -> unit pure -> unit pure) KB.t =
    let lift_bitv op =
      lift_binop op
        (ty_of_base_type info ty_a)
        (ty_of_base_type info ty_b) in
    match op with
    | ADD -> KB.return @@ lift_bitv CT.add
    | SUB -> KB.return @@ lift_bitv CT.sub
    | MUL -> KB.return @@ lift_bitv CT.mul
    | DIV -> begin
        match Patch_c.sign_of_typ ty_a, Patch_c.sign_of_typ ty_b with
        | None, _ | _, None ->
          Err.fail @@ Core_c_error (
            sprintf "DIV requires a signedness on both operands")
        | Some UNSIGNED, _ | _, Some UNSIGNED -> KB.return @@ lift_bitv CT.div
        | Some SIGNED, Some SIGNED -> KB.return @@ lift_bitv CT.sdiv
      end
    | MOD -> begin
        match Patch_c.sign_of_typ ty_a, Patch_c.sign_of_typ ty_b with
        | None, _ | _, None ->
          Err.fail @@ Core_c_error (
            sprintf "MOD requires a signedness on both operands")
        | Some UNSIGNED, _ | _, Some UNSIGNED ->
          KB.return @@ lift_bitv CT.modulo
        | Some SIGNED, Some SIGNED -> KB.return @@ lift_bitv CT.smodulo
      end
    | LAND -> KB.return @@ lift_bitv CT.logand
    | LOR -> KB.return @@ lift_bitv CT.logor
    | XOR -> KB.return @@ lift_bitv CT.logxor
    | SHL -> KB.return @@ lift_bitv CT.lshift
    | SHR  -> begin
        match Patch_c.sign_of_typ ty_a, Patch_c.sign_of_typ ty_b with
        | Some SIGNED, Some _ -> KB.return @@ lift_bitv CT.arshift
        | Some UNSIGNED, Some _ -> KB.return @@ lift_bitv CT.rshift
        | None, _ | _, None -> Err.fail @@ Core_c_error (
            sprintf "SHR requires a signedness on both operands")
      end
    | EQ  -> KB.return @@ lift_bitv CT.eq
    | NE  -> KB.return @@ lift_bitv CT.neq
    | LT -> begin
        match Patch_c.sign_of_typ ty_a, Patch_c.sign_of_typ ty_b with
        | None, None -> KB.return @@ lift_bitv CT.ult
        | None, _ | _, None ->
          Err.fail @@ Core_c_error (
            sprintf "LT requires a signedness on both operands or on none")
        | Some UNSIGNED, _ | _, Some UNSIGNED -> KB.return @@ lift_bitv CT.ult
        | Some SIGNED, Some SIGNED -> KB.return @@ lift_bitv CT.slt
      end
    | GT -> begin
        match Patch_c.sign_of_typ ty_a, Patch_c.sign_of_typ ty_b with
        | None, None -> KB.return @@ lift_bitv CT.ugt
        | None, _ | _, None ->
          Err.fail @@ Core_c_error (
            sprintf "GT requires a signedness on both operands or on none")          
        | Some UNSIGNED, _ | _, Some UNSIGNED -> KB.return @@ lift_bitv CT.ugt
        | Some SIGNED, Some SIGNED -> KB.return @@ lift_bitv CT.sgt
      end
    | LE -> begin
        match Patch_c.sign_of_typ ty_a, Patch_c.sign_of_typ ty_b with
        | None, None -> KB.return @@ lift_bitv CT.ule
        | None, _ | _, None ->
          Err.fail @@ Core_c_error (
            sprintf "LE requires a signedness on both operands or on none") 
        | Some UNSIGNED, _ | _, Some UNSIGNED -> KB.return @@ lift_bitv CT.ule
        | Some SIGNED, Some SIGNED -> KB.return @@ lift_bitv CT.sle
      end
    | GE -> begin
        match Patch_c.sign_of_typ ty_a, Patch_c.sign_of_typ ty_b with
        | None, None -> KB.return @@ lift_bitv CT.uge
        | None, _ | _, None ->
          Err.fail @@ Core_c_error (
            sprintf "GE requires a signedness on both operands or on none") 
        | Some UNSIGNED, _ | _, Some UNSIGNED -> KB.return @@ lift_bitv CT.uge
        | Some SIGNED, Some SIGNED -> KB.return @@ lift_bitv CT.sge
      end

  type 'a bitv = 'a T.bitv

  let lift_uop
      (op : 'a T.value KB.t -> 'b T.value KB.t)
      (sort_a : 'a T.Value.sort)
      (a : unit pure) : unit pure =
    let* a = a in
    let* res = op (resort sort_a a) in
    KB.return @@ T.Value.forget res

  let unop_to_pure info
      (op : Patch_c.unop)
      (ty : Patch_c.typ) : unit pure -> unit pure =
    let lift_bitv op = lift_uop op info.word_sort in
    match op with
    | MINUS -> lift_bitv CT.neg
    | LNOT -> lift_bitv CT.not
    | MEMOF ->
      let ty = ty_op_pointer_type info ty in
      let load : _ bitv -> _ bitv =
        CT.(loadw ty !!(info.endian) (var info.mem_var)) in
      lift_bitv load
    | ADDROF -> fun _ -> 
      Err.fail @@ Core_c_error "unop_to_pure: ADDROF unsupported by VIBES"

  let addr_of_var info (v : string) : unit pure =
    match Hvar.find v info.hvars with
    | None -> Err.fail @@ Err.Core_c_error
        (sprintf "addr_of_var: missing higher var %s for ADDROF expression, \
                  storage classification is required" v)
    | Some hvar -> match Hvar.value hvar with
      | Hvar.Storage {at_entry; _} -> begin
          match at_entry with
          | Hvar.(Memory (Frame (reg, off))) ->
            let* reg = try KB.return @@ Substituter.mark_reg_exn info.tgt reg with
              | Substituter.Subst_err msg -> Err.fail @@ Err.Core_c_error
                  (sprintf "addr_of_var: substitution failed: %s" msg) in
            let reg =
              T.Var.create info.word_sort @@
              T.Var.Ident.of_string @@
              Var.name reg in
            let+ a =
              CT.add (CT.var reg)
                (CT.int info.word_sort (Word.to_bitvec off)) in
            T.Value.forget a
          | Hvar.(Memory (Global addr)) ->
            let+ a = CT.int info.word_sort (Word.to_bitvec addr) in
            T.Value.forget a
          | _ -> Err.fail @@ Err.Core_c_error
              (sprintf "addr_of_var: higher var %s for ADDROF expression is \
                        not stored in a memory location." v)
        end
      | _ -> Err.fail @@ Err.Core_c_error
          (sprintf "addr_of_var: higher var %s for ADDROF expression has no \
                    storage classifier." v)

  let rec expr_to_pure
      (info : _ interp_info)
      (e : Patch_c.exp) : unit pure =
    let aux = expr_to_pure info in
    match e with
    | UNARY (ADDROF, VARIABLE (v, _), _) -> addr_of_var info @@ T.Var.name v
    | UNARY (ADDROF, UNARY (MEMOF, a, _), _) -> aux a
    | UNARY (op, a, _) ->
      let ty_a = Patch_c.typeof a in
      let* a = aux a in
      unop_to_pure info op ty_a !!a
    | BINARY (op, a, b, _) ->
      let ty_a = Patch_c.typeof a in
      let ty_b = Patch_c.typeof b in
      let* a = aux a in
      let* b = aux b in
      let* o = binop_to_pure info op ty_a ty_b in
      o !!a !!b
    | VARIABLE (v, _) -> CT.var v
    | CONST_INT (w, _) ->
      let+ i = CT.int info.word_sort @@ Word.to_bitvec w in
      T.Value.forget i
    | CAST (t, e) ->
      let t' = Patch_c.typeof e in
      let sz = Patch_c.size_of_typ info.tgt t in
      let sz' = Patch_c.size_of_typ info.tgt t' in
      let* e = aux e in
      if sz = sz' then !!e
      else
        let s = ty_of_base_type info t in
        let s' = ty_of_base_type info t' in
        let e = resort s' e in
        let+ c =
          (* No extension, just grab the lower bits. *)
          if sz < sz' then CT.low s e
          else
            (* Apply the integral promotion rules. Based on the signedness of
               each type, figure out if we need a sign extension or a zero
               extension. *)
            match Patch_c.sign_of_typ t, Patch_c.sign_of_typ t' with
            | Some SIGNED,   Some SIGNED   -> CT.signed   s e
            | Some SIGNED,   Some UNSIGNED -> CT.unsigned s e
            | Some UNSIGNED, Some SIGNED   -> CT.signed   s e
            | Some UNSIGNED, Some UNSIGNED -> CT.unsigned s e
            (* Assume unsigned. *)
            | None, _ | _, None -> CT.unsigned s e in
        T.Value.forget c

  type 'a eff = 'a T.eff

  let empty_data = T.Effect.(empty @@ Sort.data "C_NOP")

  let assign_args
      (info : _ interp_info)
      (args : unit T.value list) : (T.data eff * var list) KB.t =
    try
      List.mapi args ~f:(fun i a ->
          let r = List.nth_exn info.arg_vars i in
          CT.set r !!a, Var.reify r)
      |> KB.List.fold_right ~init:(!!empty_data, [])
        ~f:(fun (assn, r) (acc_eff, acc_args) ->
            KB.return (CT.seq assn acc_eff, r :: acc_args))
    with _ ->
      Err.fail @@
      Err.Core_c_error "Maximum number of arguments for function call \
                        was exceeded"

  let call_dst_with_name (name : string) : T.label KB.t =
    let* dst = T.Label.fresh in
    let* () = KB.provide T.Label.name dst @@ Some name in
    !!dst

  let call_dst_with_addr (addr : Bitvec.t) : T.label KB.t =
    let* dst = T.Label.fresh in
    let* () = KB.provide T.Label.addr dst @@ Some addr in
    !!dst

  let determine_call_dst
      (info : _ interp_info)
      (f : Patch_c.exp) : (T.label * T.Effect.Sort.data T.effect) KB.t =
    match f with
    | VARIABLE (v, _) ->
      let+ dst = call_dst_with_name @@ T.Var.name v in
      dst, empty_data
    | CAST (_, CONST_INT (w, _)) ->
      let+ dst = call_dst_with_addr @@ Word.to_bitvec w in
      dst, empty_data
    | _ ->
      let* fn = T.Var.fresh info.word_sort in
      let* f = expr_to_pure info f in
      let f = resort info.word_sort f in
      let* setf = CT.set fn f in
      let+ dst = call_dst_with_name @@ T.Var.name fn in
      dst, setf

  let empty_ctrl = T.Effect.(empty Sort.fall)
  let empty_blk = CT.blk T.Label.null !!empty_data !!empty_ctrl
  let data d = CT.blk T.Label.null !!d !!empty_ctrl
  let ctrl c = CT.blk T.Label.null !!empty_data !!c

  let rec stmt_to_eff
      (info : _ interp_info)
      (s : Patch_c.stmt) : unit eff =
    let aux = stmt_to_eff info in
    match s with
    | NOP -> empty_blk
    | BLOCK (_, s) -> aux s
    | ASSIGN ((v, t), e) ->
      let s = ty_of_base_type info t in
      let v = T.Var.resort v s in
      let* e = expr_to_pure info e in
      let e = resort s e in
      let* assn = CT.set v e in
      data assn
    | CALL (f, args) ->
      let* args = KB.List.map args ~f:(expr_to_pure info) in
      let* setargs, args = assign_args info args in
      let* dst, setf = determine_call_dst info f in
      let* () = provide_args dst args in
      let* () = declare_call dst in
      let* call = CT.goto dst in
      CT.blk T.Label.null CT.(seq setargs !!setf) !!call
    | CALLASSIGN ((v, _), f, args) ->
      let* args = KB.List.map args ~f:(expr_to_pure info) in
      let* setargs, args = assign_args info args in
      let* dst, setf = determine_call_dst info f in
      let* () = provide_args dst args in
      let* () = declare_call dst in
      let* call = CT.goto dst in
      let* call_blk = CT.blk T.Label.null CT.(seq setargs !!setf) !!call in
      let* retval = CT.set v @@ CT.var info.ret_var in
      let* post_blk = data retval in
      CT.seq !!call_blk !!post_blk
    | STORE (l, r) ->
      let* l = expr_to_pure info l in
      let l = resort info.word_sort l in
      let sr = ty_of_base_type info @@ Patch_c.typeof r in
      let* r = expr_to_pure info r in
      let r = resort sr r in
      let* st =
        CT.(set info.mem_var
              (storew !!(info.endian)
                 (var info.mem_var) l r)) in
      data st
    | SEQUENCE (s1, s2) ->
      let* s1 = aux s1 in
      let* s2 = aux s2 in
      CT.seq !!s1 !!s2
    | IF (cond, st, sf) ->
      let* cond = expr_to_pure info cond in
      let cond = resort T.Bool.t cond in
      let* st = aux st in
      let* sf = aux sf in
      CT.branch cond !!st !!sf
    | GOTO label when String.(is_prefix ~prefix:"L_0x" label) ->
      let label = String.(chop_prefix_exn ~prefix:"L_" label) in
      let dst = CT.int info.word_sort Bitvec.(!$ label) in
      let* jmp = CT.jmp dst in
      ctrl jmp
    | GOTO label ->
      let label = T.Label.for_name label in
      let* goto = KB.(label >>= CT.goto) in
      ctrl goto

  and body_to_eff info ((_, stmt) : Patch_c.t) : unit eff =
    stmt_to_eff info stmt

  let c_patch_to_eff (hvars : Hvar.t list) (tgt : T.target)
      (patch : Cabs.definition) : unit eff =
    let* body = Patch_c.translate patch ~target:tgt in
    let* info = mk_interp_info hvars tgt in
    body_to_eff info body

end
