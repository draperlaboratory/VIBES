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
 * - C has complex types and operations over those types. We punt on this
 *   by having every variable be a machine word by default. Since [Core]
 *   uses phantom types to be type-safe, we must re-sort all the
 *   arguments and cast them to "unit" (top) type using [resort].
 *
 * - C has operations whose semantics are contingent on the types of the
 *   operands. We cheat a bit with this one, only allowing such
 *   operations if the operands are explicitly cast beforehand, thereby
 *   disambiguating the operations semantics.
 *
 * - Core does not have a construct for function calls (in particular,
 *   function calls with arguments!). We solve half of the problem by
 *   tagging the [tid] of a function call *destination* with a semantic
 *   predicate marking it as a function call. Note that the instruction
 *   selector will need to then read that tag when deciding whether to
 *   generate special instructions.
 *   We do not yet solve the second half of the issue, namely how to
 *   bundle the function arguments into the jump operation.
 *
 * The width of machine words, and the memory layout are stored in the
 * [interp_info] type, which can be generated by reading the relevant values
 * from the [Theory.target] which corresponds to the target architecture.
 *
 * Note that a bit of de-sugaring happens, e.g. [a[x]] to [*(a + x)] etc.
 *
 *
 * Jumps to concrete addresses are not possible directly in C, we support
 * an alternate syntax: [goto L_some_hex_number], which in C syntax is an
 * ordinary [goto] to a label, but which is handled specially by our
 * processor.
 *
 *
 * We don't handle local variables, and many other things.
 ***********************************************************************)

open Core_kernel
open Bap_core_theory
open Bap.Std
open Cabs
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
    word_sort : 'a T.Bitv.t T.Value.sort;
    byte_sort : 'b T.Bitv.t T.Value.sort;
    mem_var   : ('a, 'b) T.Mem.t T.var;
    endian    : T.Bool.t T.value;
    ret_var   : unit T.var;
    arg_vars  : unit T.var list;
    hvars     : Hvar.t list;
  }

  let mk_interp_info (hvars : Hvar.t list)
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
      word_sort = word_sort;
      byte_sort = byte_sort;
      mem_var = T.Target.data tgt;
      endian;
      ret_var = Option.value_exn ret_var;
      arg_vars = Set.to_list arg_vars;
      hvars;
    }

  let char_ty = Theory.Bitv.define 8
  let short_ty = Theory.Bitv.define 16
  let long_ty = Theory.Bitv.define 32
  let long_long_ty = Theory.Bitv.define 64

  let ty_of_base_type info (c_ty : base_type) : _ T.Bitv.t T.Value.sort =
    match c_ty with
    | CHAR _ -> char_ty
    | INT (SHORT, _) -> short_ty
    | INT (LONG, _) -> long_ty
    | INT (LONG_LONG, _) -> long_long_ty
    | _ -> info.word_sort

  let ty_op_pointer_type info (c_ty : base_type) : _ T.Bitv.t T.Value.sort =
    match c_ty with
    | PTR ty -> ty_of_base_type info ty
    | _ -> info.word_sort

  (* TODO: we'll need to pass around some additional info here. *)
  let infer (exp : expression) : base_type =
    match exp with
    | CAST (ty, _) -> ty
    | _ -> NO_TYPE

  let is_signed (ty : base_type) : sign =
    match ty with
    | CHAR s -> s
    | INT (_, s) -> s
    | _ -> NO_SIGN

  let resort sort v =
    let error = "Incorrect argument sort!" in
    T.Value.resort (fun _ -> Some sort) v
    |> Option.value_exn ~message:error
    |> KB.return

  type 'a pure = 'a T.pure

  let binop_to_pure info (op : Cabs.binary_operator)
      (ty_a : base_type)
      (ty_b : base_type)
    : unit pure -> unit pure -> unit pure =
    let lift_binop op sort_a sort_b (a : unit pure) (b : unit pure) : unit pure =
      let* a = a in
      let* b = b in
      let* res = op (resort sort_a a) (resort sort_b b) in
      KB.return @@ T.Value.forget res
    in
    let lift_bitv op = lift_binop op info.word_sort info.word_sort in
    match op with
    | ADD -> lift_bitv CT.add
    | SUB -> lift_bitv CT.sub
    | MUL -> lift_bitv CT.mul
    | DIV -> lift_bitv CT.div
    | SHL -> lift_bitv CT.lshift
    (* Use arithmetic shift by default *)
    | SHR  ->
      begin
        match is_signed ty_a with
        | SIGNED -> lift_bitv CT.arshift
        | UNSIGNED -> lift_bitv CT.rshift
        | NO_SIGN -> fun _ _ ->
          Err.fail @@ Err.Core_c_error
            "binop_to_pure: argument must be explicitely cast \
             to a signed or unsigned type!"
      end
    | EQ  -> lift_bitv CT.eq
    | NE  -> lift_bitv CT.neq
    (* FIXME: use unsigned by default? *)
    | LT  ->
      begin
        match is_signed ty_a, is_signed ty_b with
        | SIGNED, SIGNED -> lift_bitv CT.slt
        | UNSIGNED, UNSIGNED -> lift_bitv CT.ult
        | _ -> fun _ _ ->
          Err.fail @@ Err.Core_c_error
            "binop_to_pure: arguments must be explicitely cast \
             to equal signed or unsigned types!"
      end
    | GT  ->
      begin
        match is_signed ty_a, is_signed ty_b with
        | SIGNED, SIGNED -> lift_bitv CT.sgt
        | UNSIGNED, UNSIGNED -> lift_bitv CT.ugt
        | _ -> fun _ _ ->
          Err.fail @@ Err.Core_c_error
            "binop_to_pure: arguments must be explicitely cast \
             to equal signed or unsigned types!"
      end
    | LE  ->
      begin
        match is_signed ty_a, is_signed ty_b with
        | SIGNED, SIGNED -> lift_bitv CT.sle
        | UNSIGNED, UNSIGNED -> lift_bitv CT.ule
        | _ -> fun _ _ ->
          Err.fail @@ Err.Core_c_error
            "binop_to_pure: arguments must be explicitely cast \
             to equal signed or unsigned types!"
      end
    | GE  ->
      begin
        match is_signed ty_a, is_signed ty_b with
        | SIGNED, SIGNED -> lift_bitv CT.sge
        | UNSIGNED, UNSIGNED -> lift_bitv CT.uge
        | _ -> fun _ _ ->
          Err.fail @@ Err.Core_c_error
            "binop_to_pure: arguments must be explicitely cast \
             to equal signed or unsigned types!"
      end
    | AND
    | OR
    | MOD
    | BAND
    | BOR
    | XOR
    | ADD_ASSIGN
    | SUB_ASSIGN
    | MUL_ASSIGN
    | DIV_ASSIGN
    | MOD_ASSIGN
    | BAND_ASSIGN
    | BOR_ASSIGN
    | XOR_ASSIGN
    | SHL_ASSIGN
    | SHR_ASSIGN
    | ASSIGN
      -> fun _ _ ->
        Err.fail @@
        Err.Core_c_error "binop_to_pure: binary operator unsupported by VIBES"

  type 'a bitv = 'a T.bitv

  let unop_to_pure info (op : Cabs.unary_operator)
      (ty : base_type)
    : unit pure -> unit pure =
    let lift_uop op sort_a (a : unit pure) : unit pure =
      let* a = a in
      let* res = op (resort sort_a a) in
      KB.return @@ T.Value.forget res
    in
    let lift_bitv op = lift_uop op info.word_sort in
    match op with
    | MEMOF ->
      (* FIXME: use endianness here *)
      let ty = ty_op_pointer_type info ty in
      let load : _ bitv -> _ bitv =
        CT.(loadw ty !!(info.endian) (var info.mem_var)) in
      lift_bitv load
    | ADDROF
    | MINUS
    | PLUS
    | NOT
    | BNOT
    | PREINCR
    | PREDECR
    | POSINCR
    | POSDECR
      -> fun _ ->
        Err.fail @@
        Err.Core_c_error "unop_to_pure: unary operator unsupported by VIBES"

  let constant_to_pure info (c :Cabs.constant) : unit pure =
    match c with
    | CONST_INT s ->
      let* res = CT.int info.word_sort Bitvec.(!$ s) in
      KB.return @@ T.Value.forget res
    | CONST_FLOAT _
    | CONST_CHAR _
    | CONST_STRING _
    | CONST_COMPOUND _ ->
      Err.fail @@
      Err.Core_c_error "constant_to_pure: constant unsupported by VIBES"

  let addr_of_var info (v : string)  : unit pure =
    match Hvar.find v info.hvars with
    | None -> Err.fail @@ Err.Core_c_error
        (sprintf "expr_to_pure: missing higher var %s for ADDROF expression, \
                  storage classification is required" v)
    | Some hvar -> match Hvar.value hvar with
      | Hvar.Storage {at_entry; _} -> begin
        match at_entry with
        | Hvar.(Memory (Frame (reg, off)))  ->
            let reg = T.Var.create info.word_sort @@ T.Var.Ident.of_string reg in
            let+ a =
              CT.add (CT.var reg)
                (CT.int info.word_sort (Word.to_bitvec off)) in
            T.Value.forget a
        | Hvar.(Memory (Global addr)) ->
          let+ a = CT.int info.word_sort (Word.to_bitvec addr) in
          T.Value.forget a
        | _ -> Err.fail @@ Err.Core_c_error
            (sprintf "expr_to_pure: higher var %s for ADDROF expression is \
                      not stored in a memory location." v)
      end
      | _ -> Err.fail @@ Err.Core_c_error
          (sprintf "expr_to_pure: higher var %s for ADDROF expression has no \
                    storage classifier." v)


  let expr_to_pure info (e : Cabs.expression) (var_map : var_map) : unit pure =
    let rec aux e =
      match e with
      | UNARY (ADDROF, VARIABLE v) -> addr_of_var info v
      | UNARY (ADDROF, UNARY (MEMOF, a)) -> aux a
      | UNARY (op, a) ->
        let ty_a = infer a in
        let* a = aux a in
        unop_to_pure info op ty_a !!a
      | BINARY (op, a, b) ->
        let ty_a = infer a in
        let ty_b = infer b in
        let* a = aux a in
        let* b = aux b in
        binop_to_pure info op ty_a ty_b !!a !!b
      | INDEX (a, i) ->
        (* Some minor hackery here: turn a[i] into *(a + i) *)
        let index_exp = UNARY (MEMOF, BINARY (ADD, a, i)) in
        aux index_exp
      | VARIABLE x ->
        let* v = match String.Map.find var_map x with
          | Some v -> !!v
          | None -> Err.(fail @@ Other (
              sprintf "Core_c.expr_to_pure: var %s was not declared" x)) in
        let* vv = CT.var v in
        !!vv
      | CONSTANT c ->
        constant_to_pure info c
      (* FIXME: for now, casts are simply for tagging subterms *)
      | CAST (_, e) -> aux e
      | _ ->
        let e_str = Utils.print_c (fun e -> Cprint.print_expression e 0) e in
        let msg =
          Format.sprintf "FrontC produced expression unsuppored by VIBES: %s"
            e_str in
        Err.fail @@ Err.Core_c_error msg
    in
    aux e

  type 'a eff = 'a T.eff

  let empty_data = T.Effect.empty (T.Effect.Sort.data "C_NOP")

  let assign_args info (var_map : var_map)
      (args : Cabs.expression list) : (T.data eff * var list) KB.t =
    let* args =
      KB.List.map args ~f:(fun e -> KB.return @@ expr_to_pure info e var_map) in
    try
      List.mapi args ~f:(fun i a ->
          let r = List.nth_exn info.arg_vars i in
          CT.set r a, Var.reify r)
      |> KB.List.fold_right ~init:(!!empty_data, [])
        ~f:(fun (assn, r) (acc_eff, acc_args) ->
            KB.return (CT.seq assn acc_eff, r :: acc_args))
    with _ ->
      Err.fail @@
      Err.Core_c_error "Maximum number of arguments for function call \
                        was exceeded"
  
  let stmt_to_eff info (s : Cabs.statement) var_map : unit eff =
    let empty_ctrl = T.Effect.empty T.Effect.Sort.fall in
    let* empty_blk = CT.blk T.Label.null !!empty_data !!empty_ctrl in
    let data d = CT.blk T.Label.null !!d !!empty_ctrl in
    let ctrl c = CT.blk T.Label.null !!empty_data !!c in
    let find_var v = match String.Map.find var_map v with
      | Some v -> !!v
      | None -> Err.(fail @@ Other (
          sprintf "Core_c.stmt_to_eff: var %s was not declared" v)) in
    let rec aux s : unit eff =
      match s with
      | NOP ->
        KB.return empty_blk
      (* FIXME: handle all "assignment-like" operations in a seperate function *)
      | COMPUTATION (BINARY (ASSIGN, VARIABLE lval, CALL (VARIABLE f, args))) ->
        let* arg_assignments, args = assign_args info var_map args in
        let* lval = find_var lval in
        let* dst = T.Label.for_name ~package:"core-c" f in
        let* () = provide_args dst args in
        let* () = declare_call dst in
        let* call = CT.goto dst in
        let* call_blk =
          CT.blk T.Label.null arg_assignments !!call in
        let* retval = CT.set lval @@ CT.var info.ret_var in
        let* post_blk = data retval in
        CT.seq !!call_blk !!post_blk
      | COMPUTATION (BINARY (ASSIGN, UNARY (MEMOF, VARIABLE lval), CALL (VARIABLE f, args))) ->
        let* arg_assignments, args = assign_args info var_map args in
        let* lval = find_var lval in
        let* dst = T.Label.for_name ~package:"core-c" f in
        let* () = provide_args dst args in
        let* () = declare_call dst in
        let* call = CT.goto dst in
        let* call_blk =
          CT.blk T.Label.null arg_assignments !!call in
        (* XXX: maybe look at the type instead of defaulting to the word_sort *)
        let* retval =
          CT.(set info.mem_var
                (storew !!(info.endian) (var info.mem_var)
                   (var (T.Var.resort lval info.word_sort))
                   (var (T.Var.resort info.ret_var info.word_sort)))) in
        let* post_blk = data retval in
        CT.seq !!call_blk !!post_blk
      | COMPUTATION (BINARY (ASSIGN, VARIABLE lval, rval)) ->
        let* lval = find_var lval in
        let* rval = expr_to_pure info rval var_map in
        let* assign = CT.set lval !!rval in
        data assign
      (* FIXME: handle general calls with arguments *)
      (* FIXME: should we allow calls to concrete addresses?
         In that case, we need a new concrete syntax for jumps.
      *)
      | COMPUTATION (CALL (CONSTANT(CONST_INT s), args)) ->
        let* arg_assignments, args = assign_args info var_map args in
        let* dst = T.Label.for_addr ~package:"core-c" @@ Bitvec.(!$s) in
        let* () = provide_args dst args in
        let* () = declare_call dst in
        let* call = CT.goto dst in
        let* call_blk = CT.blk T.Label.null arg_assignments !!call in
        !!call_blk
      | COMPUTATION (CALL (VARIABLE f, args)) ->
        let* arg_assignments, args = assign_args info var_map args in
        let* dst = T.Label.for_name ~package:"core-c" f in
        let* () = provide_args dst args in
        let* () = declare_call dst in
        let* call = CT.goto dst in
        let* call_blk = CT.blk T.Label.null arg_assignments !!call in
        !!call_blk
      | GOTO label when String.(is_prefix ~prefix:"L_0x" label) ->
        let label = String.(chop_prefix_exn ~prefix:"L_" label) in
        let dst = CT.int info.word_sort Bitvec.(!$ label) in
        let* jmp = CT.jmp dst in
        ctrl jmp
      | GOTO label ->
        let label = T.Label.for_name label in
        let* goto = KB.(label >>= CT.goto) in
        ctrl goto
      | IF (c, true_br, false_br) ->
        let c = expr_to_pure info c var_map in
        (* Downcast [c] to Bool.t *)
        let c = KB.map c
            ~f:(fun c ->
                T.Value.resort (fun _ -> Some T.Bool.t) c
                |> Option.value_exn)
        in
        let* true_eff = aux true_br in
        let* false_eff = aux false_br in
        let* branch = CT.branch c !!true_eff !!false_eff in
        KB.return branch
      | SEQUENCE (s1,s2) ->
        let* eff1 = aux s1 in
        let* eff2 = aux s2 in
        let* eff = CT.seq !!eff1 !!eff2 in
        KB.return eff
      (* FIXME: allow additional var defs? *)
      | BLOCK ([],s) ->
        let* eff = aux s in
        KB.return eff
      | _ ->
        let s_str = Utils.print_c Cprint.print_statement s in
        let err =
          Format.asprintf "stmt_to_eff: statement %s unsupported by VIBES" s_str
        in
        Err.fail @@ Err.Core_c_error err
    in
    let* eff = aux s in
    KB.return eff



  let add_def info (def : Cabs.definition) (var_map : var_map) : var_map KB.t =
    match def with
    (* FIXME: handle bitwidth here? *)
    | DECDEF (_typ, _storage, names) ->
      let decl s =
        let ty = info.word_sort in
        T.Var.define ty s |> T.Var.forget
      in
      KB.return @@ List.fold names ~init:var_map
        ~f:(fun map name ->
            let (name, _, _, _) = name in
            String.Map.set map ~key:name ~data:(decl name))
    | _ -> Err.(fail @@ Other "Core_c.add_def: expected DECDEF")

  let defs_to_map info (defs : Cabs.definition list) : var_map KB.t =
    KB.List.fold defs ~init:String.Map.empty
      ~f:(fun map def -> add_def info def map)

  let body_to_eff info ((defs, stmt) : Cabs.body) : unit eff =
    let* var_map = defs_to_map info defs in
    stmt_to_eff info stmt var_map

  let c_patch_to_eff (hvars : Hvar.t list) (tgt : T.target)
      (patch : Cabs.definition) : unit eff =
    let* info = mk_interp_info hvars tgt in
    match patch with
    | FUNDEF (_, b) -> body_to_eff info b
    | _ -> Err.fail @@
      Err.Core_c_error "c_patch_to_eff: unexpected patch shape; expected a single fundef!"

end
