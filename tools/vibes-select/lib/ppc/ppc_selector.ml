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

open Core
open Bap.Std
open Bap_core_theory

module T = Theory
module Tags = Vibes_bir.Tags
module Ir = Vibes_ir.Types
module Helpers = Vibes_bir.Helpers
module Linear = Vibes_linear_ssa.Utils
module Naming = Vibes_higher_vars.Substituter.Naming
module Ops = Ppc_ops
module Pre = Types.Preassign
module Param = Types.Call_params

open KB.Syntax
open Types.Sel

(* CR0 seems like a reasonable choice. *)
let cr_num : int = 0

let fail msg = KB.fail @@ Errors.Selector_error msg

let word_ty : typ = Bil.Imm 32
let bit_ty : typ = Bil.Imm 1
let mem_ty : typ = Bil.Mem (`r32, `r8)

let temp (t : typ) : Ir.Opvar.t KB.t =
  let s = Helpers.sort_of_typ t in
  let+ v = T.Var.fresh s in
  Ir.Opvar.create @@ Var.reify v

let var_temp (t : typ) : Ir.Operand.t KB.t =
  let+ v = temp t in
  Ir.Operand.Var v

let void_temp (t : typ) : Ir.Operand.t KB.t =
  let+ v = temp t in
  Ir.Operand.Void v

(* Use this to generate a temp var to store the result of a
   comparison. *)
let cr_temp : Ir.Operand.t KB.t =
  let+ v = temp @@ Imm 4 in
  let name = Format.sprintf "CR%d" cr_num in
  let v = {v with preassign = Some (Var.create name @@ Imm 4)} in
  Ir.Operand.Var v

(* Helper data structure for generating conditional branches. *)
module Branch = struct

  (* A thunk that accepts the condition `cnd`, the flag pseudo-operand
     `flg` and returns the corresponding branch instruction, along with
     the fake destination operand. `flg` is to mark the status flags
     as a dependency of the branch instruction. *)
  type t =
    cnd:Ops.cond ->
    flg:Ir.Operand.t ->
    (Ir.Operation.t * Ir.Operand.t) KB.t

  let create (dst : Ir.Operand.t) : t = fun ~cnd ~flg ->
    let+ tmp = void_temp bit_ty in
    let c = Ops.b () ~cnd:(Some cnd) in
    let op = Ir.Operation.create_simple c tmp [flg; dst] in
    op, tmp

end

let mov_const ?(eff : eff = empty_eff) (c : word) (x : Ir.Operand.t) : eff KB.t =
  match Word.to_int_exn c with
  | n when (n >= 0xFFFF_8000 && n <= 0xFFFF_FFFF) || (n <= 0xFFFF) ->
    let o = Ir.Operation.create_simple Ops.li x [Const c] in
    !!(instr o eff)
  | _ ->
    let+ t = var_temp @@ Imm 32 in
    let width = Word.bitwidth c in
    let sh = Ir.Operand.Const (Word.of_int ~width 16) in
    let mask = Word.of_int ~width 0xFFFF in
    let cl = Ir.Operand.Const Word.(c land mask) in
    let u = Ir.Operation.create_simple Ops.lis t [sh] in
    let o = Ir.Operation.create_simple Ops.ori x [t; cl] in
    instr o @@ instr u eff

let mov (l : Ir.Operand.t) (r : pure) : eff KB.t =
  match l, r.value with
  | Var _, Var _ when not @@ List.is_empty r.eff.data -> begin
      (* FIXME: absolute hack! if we have vars here, we can assume
         that the last operation assigned to a temporary, and we can
         just replace that temporary with the known destination, and
         return that as the effect. *)
      match r.eff.data with
      | [] -> assert false (* excluded by the guard above *)
      | op :: ops ->
        !!{r.eff with data = {op with lhs = [l]} :: ops}
    end
  | Var _, Var _ ->
    let mov = Ir.Operation.create_simple Ops.mr l [r.value] in
    !!(instr mov r.eff)
  | Var _, Const w -> mov_const w l ~eff:r.eff
  | Void _, Void _ when not @@ List.is_empty r.eff.data -> begin
      (* Same hack as above, but with void operands. *)
      match r.eff.data with
      | [] -> assert false
      | op :: ops -> !!{r.eff with data = {op with lhs = [l]} :: ops}
    end
  | _ ->
    fail @@ Format.asprintf
      "mov: unexpected arguments (%a, %a)"
      Ir.Operand.pp l Ir.Operand.pp r.value

let var (v : var) : pure = {
  value = Var (Ir.Opvar.create v);
  eff = empty_eff;
}

let mem (v : var) : pure = {
  value = Void (Ir.Opvar.create v);
  eff = empty_eff
}

let const (c : word) : pure = {
  value = Const c;
  eff = empty_eff
}

let uop (o : Ir.opcode) (ty : typ) (arg : pure) : pure KB.t =
  let* res = var_temp ty in
  match arg.value with
  | Const w ->
    let* tmp = var_temp word_ty in
    let+ eff = mov_const w tmp ~eff:arg.eff in
    let op = Ir.Operation.create_simple o res [tmp] in
    {value = res; eff = instr op eff}
  | _ ->
    let op = Ir.Operation.create_simple o res [arg.value] in
    !!{value = res; eff = instr op arg.eff}

let can_be_swapped : binop -> bool = function
  | LT | SLT | LE | SLE -> true
  | _ -> false

let commutative : binop -> bool = function
  | PLUS | TIMES | AND | OR | XOR | EQ | NEQ -> true
  | MINUS
  | DIVIDE
  | SDIVIDE
  | MOD
  | SMOD
  | LSHIFT
  | RSHIFT
  | ARSHIFT
  | LT
  | SLT
  | LE
  | SLE -> false

let binop
    ?(oi : Ir.opcode option)
    (o : Ir.opcode)
    (ty : typ)
    (l : pure)
    (r : pure) : pure KB.t =
  let* res = var_temp ty in
  let eff = l.eff @. r.eff in
  let o = match r.value with
    | Const _ -> Option.value oi ~default:o
    | _ -> o in
  match r.value with
  | Const w when Word.to_int_exn w > 0xFFFF ->
    (* For binops that allow constant operands, the limit seems
       to be 16 bits according to the manual. *)
    let* tmp = var_temp word_ty in
    let+ eff = mov_const w tmp ~eff in
    let op = Ir.Operation.create_simple o res [l.value; tmp] in
    {value = res; eff = instr op eff}
  | _ ->
    let op = Ir.Operation.create_simple o res [l.value; r.value] in
    !!{value = res; eff = instr op eff}

let ternop
    (o : Ir.opcode)
    (ty : typ)
    (x : pure)
    (y : pure)
    (z : pure) : pure KB.t =
  let+ res = var_temp ty in
  let op = Ir.Operation.create_simple o res [x.value; y.value; z.value] in
  let eff = x.eff @. y.eff @. z.eff in
  {value = res; eff = instr op eff}

let add (l : pure) (r : pure) : pure KB.t =
  binop Ops.add word_ty l r ~oi:Ops.addi

let and_ (l : pure) (r : pure) : pure KB.t =
  binop Ops.and_ word_ty l r ~oi:Ops.andi

let neg (x : pure) : pure KB.t = uop Ops.neg word_ty x
let lognot (x : pure) : pure KB.t = uop Ops.not word_ty x

let mul (l : pure) (r : pure) : pure KB.t =
  binop Ops.mullw word_ty l r ~oi:Ops.mulli

let or_ (l : pure) (r : pure) : pure KB.t =
  binop Ops.or_ word_ty l r ~oi:Ops.ori

let sub (l : pure) (r : pure) : pure KB.t =
  binop Ops.sub word_ty l r ~oi:Ops.subi

let slw (l : pure) (r : pure) : pure KB.t =
  binop Ops.slw word_ty l r ~oi:Ops.slwi

let srw (l : pure) (r : pure) : pure KB.t =
  binop Ops.srw word_ty l r ~oi:Ops.srwi

let sraw (l : pure) (r : pure) : pure KB.t =
  binop Ops.sraw word_ty l r ~oi:Ops.srawi

let xor (l : pure) (r : pure) : pure KB.t =
  binop Ops.xor word_ty l r ~oi:Ops.xori

let load_op (bits : int) : (Ir.opcode * Ir.opcode) KB.t =
  if bits = 32 then !!Ops.(lwzx, lwz)
  else if bits = 16 then !!Ops.(lhzx, lhz)
  else if bits = 8 then !!Ops.(lbzx, lbz)
  else fail @@ Format.sprintf
      "load_op: loading a bit-width that is not \
       8, 16, or 32 (got %d)" bits

let store_op (bits : int) : (Ir.opcode * Ir.opcode) KB.t =
  if bits = 32 then !!Ops.(stwx, stw)
  else if bits = 16 then !!Ops.(sthx, sth)
  else if bits = 8 then !!Ops.(stbx, stb)
  else fail @@ Format.sprintf
      "store_op: loading a bit-width that is not \
       8, 16, or 32 (got %d)" bits

let const_off_fits (w : word) : bool =
  let i = Word.to_int_exn w in
  i <= 0x7FFF || i >= 0xFFFF8000

let load (bits : int) (mem : pure) (off : pure) (loc : pure) : pure KB.t =
  let* o, oi = load_op bits in
  let* loc = match loc.value with
    | Const w ->
      let* tmp = var_temp word_ty in
      let+ c = mov_const w tmp ~eff:loc.eff in
      {value = tmp; eff = c}
    | _ -> !!loc in
  match off.value with
  | Const w when not @@ const_off_fits w ->
    let* tmp = var_temp word_ty in
    let* res = var_temp word_ty in
    let+ eff = mov_const w tmp ~eff:(mem.eff @. off.eff @. loc.eff) in
    let op = Ir.Operation.create_simple o res [loc.value; tmp] in
    {value = res; eff = instr op eff}
  | Const _ -> ternop oi word_ty mem off loc
  | _ -> ternop o word_ty mem loc off

let store
    (bits : int)
    (mem : pure)
    (value : pure)
    (off : pure)
    (loc : pure) : pure KB.t =
  let* o, oi = store_op bits in
  let* res = void_temp mem_ty in
  let* loc = match loc.value with
    | Const w ->
      let* tmp = var_temp word_ty in
      let+ c = mov_const w tmp ~eff:loc.eff in
      {value = tmp; eff = c}
    | _ -> !!loc in
  let eff = loc.eff @. off.eff @. value.eff @. mem.eff in
  let op = match off.value with
    | Const w as c when const_off_fits w -> fun v ->
      let op = Ir.Operation.create_simple oi res [mem.value; v; c; loc.value] in
      !!(instr op eff)
    | Const w -> fun v ->
      let* tmp = var_temp word_ty in
      let+ eff = mov_const w tmp ~eff in
      let op = Ir.Operation.create_simple o res [mem.value; v; loc.value; tmp] in
      instr op eff
    | x -> fun v ->
      let op = Ir.Operation.create_simple o res [mem.value; v; loc.value; x] in
      !!(instr op eff) in
  let+ eff = match value.value with
    | Var _ as v -> op v
    | Const w ->
      let* tmp = var_temp word_ty in
      let* eff = mov_const w tmp in
      let+ st = op tmp in
      st @. eff
    | _ ->
      fail @@ Format.asprintf
        "store: unsupported `value` operand %a"
        Ir.Operand.pp value.value in
  {value = res; eff}

let binop_cmp
    ?(signed : bool = true)
    (cond : Ops.cond)
    (l : pure)
    (r : pure)
    ~(branch : Branch.t option) : pure KB.t =
  let* tmp_flag = cr_temp in
  let eff = l.eff @. r.eff in
  let o = match r.value with
    | Const _ when signed -> Ops.cmpwi
    | Const _ -> Ops.cmplwi
    | _ when signed -> Ops.cmpw
    | _ -> Ops.cmplw in
  match branch with
  | Some generate ->
    let* () = match eff.ctrl with
      | [] -> !!()
      | _ ->
        fail "binop_cmp: encountered a branch with non-empty \
              ctrl semantics" in
    let cmp = Ir.Operation.create_simple o tmp_flag [l.value; r.value] in
    let+ br, value = generate ~cnd:cond ~flg:tmp_flag in
    {value; eff = control br @@ instr cmp eff}
  | None ->
    (* Get the result of the comparison by testing a particular bit of the
       condition register. *)
    let* tmp1 = var_temp word_ty in
    let cmp = Ir.Operation.create_simple o tmp_flag [l.value; r.value] in
    let cr = Ir.Operation.create_simple Ops.mfcr tmp1 [tmp_flag] in
    let bitnum = match cond with
      | EQ | NE -> 2
      | GT | LE -> 1
      | LT | GE -> 0 in
    let b = Ir.Operand.Const (Word.of_int ~width:32 (bitnum + 1)) in
    let m = Ir.Operand.Const (Word.of_int ~width:32 31) in
    let* tmp2 = var_temp word_ty in
    let rot = Ir.Operation.create_simple Ops.rlwinm tmp2 [tmp1; b; m; m] in
    let+ value, ops = match cond with
      | NE | LE | GE ->
        let+ tmp3 = var_temp word_ty in
        let one = Ir.Operand.Const (Word.one 32) in
        let x = Ir.Operation.create_simple Ops.xori tmp3 [tmp2; one] in
        tmp3, [x; rot; cr; cmp]
      | _ -> !!(tmp2, [rot; cr; cmp]) in
    {value; eff = List.fold_right ops ~init:eff ~f:instr}

let equals ~(branch : Branch.t option) : pure -> pure -> pure KB.t =
  binop_cmp EQ ~branch

let not_equals ~(branch : Branch.t option) : pure -> pure -> pure KB.t =
  binop_cmp NE ~branch

let less_than
    (l : pure)
    (r : pure)
    ~(branch : Branch.t option) : pure KB.t = match l.value with
  | Const _ -> binop_cmp GT r l ~signed:false ~branch
  | _ -> binop_cmp LT l r ~signed:false ~branch

let less_or_equal
    (l : pure)
    (r : pure)
    ~(branch : Branch.t option) : pure KB.t = match l.value with
  | Const _ -> binop_cmp GE r l ~signed:false ~branch
  | _ -> binop_cmp LE l r ~signed:false ~branch

let signed_less_than
    (l : pure)
    (r : pure)
    ~(branch : Branch.t option) : pure KB.t = match l.value with
  | Const _ -> binop_cmp GT r l ~branch
  | _ -> binop_cmp LT l r ~branch 

let signed_less_or_equal
    (l : pure)
    (r : pure)
    ~(branch : Branch.t option) : pure KB.t = match l.value with
  | Const _ -> binop_cmp GE r l ~branch
  | _ -> binop_cmp LE l r ~branch

let goto
    ?(is_call : bool = false)
    (tgt : Ir.Operand.t)
    (call_params : Ir.Operand.t list) : eff KB.t =
  let c = if is_call then Ops.bl else Ops.b () in
  let+ tmp_branch = void_temp bit_ty in
  let op = Ir.Operation.create_simple c tmp_branch (tgt :: call_params) in
  control op empty_eff

let sel_binop
    (o : binop)
    ~(branch : Branch.t option) : (pure -> pure -> pure KB.t) KB.t =
  match o with
  | PLUS -> !!add
  | MINUS -> !!sub
  | TIMES -> !!mul
  | LSHIFT -> !!slw
  | RSHIFT -> !!srw
  | ARSHIFT ->  !!sraw
  | AND -> !!and_
  | OR -> !!or_
  | EQ -> !!(equals ~branch)
  | NEQ -> !!(not_equals ~branch)
  | LT -> !!(less_than ~branch)
  | LE -> !!(less_or_equal ~branch)
  | SLT -> !!(signed_less_than ~branch)
  | SLE -> !!(signed_less_or_equal ~branch)
  | XOR -> !!xor
  | DIVIDE | SDIVIDE | MOD | SMOD ->
    fail @@ Format.sprintf
      "sel_binop: unsupported operation %s"
      (Bil.string_of_binop o)

let sel_unop (o : unop) : (pure -> pure KB.t) KB.t = match o with
  | NOT -> !!lognot
  | NEG -> !!neg

let rec select_exp
    ?(branch : Branch.t option = None)
    ?(lhs : var option = None)
    (e : exp) : pure KB.t =
  let exp = select_exp ~branch:None ~lhs:None in
  match e with
  | Load (mem, BinOp (PLUS, a, Int w), _, size)
  | Load (mem, BinOp (PLUS, Int w, a), _, size) ->
    let* mem = exp mem in
    let* loc = exp a in
    load (Size.in_bits size) mem (const w) loc
  | Load (mem, BinOp (PLUS, a, b), _, size) ->
    let* mem = exp mem in
    let* loc = exp a in
    let* off = exp b in
    load (Size.in_bits size) mem off loc
  | Load (mem, BinOp (MINUS, a, Int w), _, size) ->
    let* mem = exp mem in
    let* loc = exp a in
    load (Size.in_bits size) mem (const @@ Word.neg w) loc
  | Load (mem, Int w, _, size) ->
    let* mem = exp mem in
    load (Size.in_bits size) mem (const @@ Word.zero 32) (const w)
  | Load (mem, loc, _, size) ->
    let* mem = exp mem in
    let* loc = exp loc in
    let z = const @@ Word.zero 32 in
    load (Size.in_bits size) mem z loc
  | Store (mem, BinOp (PLUS, a, Int w), value, _, size)
  | Store (mem, BinOp (PLUS, Int w, a), value, _, size) ->
    let* mem = exp mem in
    let* loc = exp a in
    let* value = exp value in
    store (Size.in_bits size) mem value (const w) loc
  | Store (mem, BinOp (PLUS, a, b), value, _, size) ->
    let* mem = exp mem in
    let* loc = exp a in
    let* off = exp b in
    let* value = exp value in
    store (Size.in_bits size) mem value off loc
  | Store (mem, BinOp (MINUS, a, Int w), value, _, size) ->
    let* mem = exp mem in
    let* loc = exp a in
    let* value = exp value in
    store (Size.in_bits size) mem value (const @@ Word.neg w) loc
  | Store (mem, Int w, value, _, size) ->
    let* mem = exp mem in
    let* value = exp value in
    store (Size.in_bits size) mem value (const @@ Word.zero 32) (const w)
  | Store (mem, loc, value, _, size) ->
    let* mem = exp mem in
    let* loc = exp loc in
    let* value = exp value in
    let z = const @@ Word.zero 32 in
    store (Size.in_bits size) mem value z loc
  | BinOp (TIMES, Int w, x) | BinOp (TIMES, x, Int w) ->
    let* x = exp x in
    let i = Word.to_int_exn w in
    (* Power of two can be simplified to a left shift. *)
    if Int.is_pow2 i then
      let zero = const @@ Word.zero 32 in
      let sh = Int.ctz i in
      (* Greater than 31 is not encodable, but that also just means we're
         shifting out every bit, so the result is zero. *)
      if sh <= 31 then
        slw x @@ const @@ Word.of_int sh ~width:32
      else !!zero
    else mul x @@ const w
  | BinOp (o, a, b) ->
    let* a = exp a in
    let* b = exp b in
    let move w eff =
      let* tmp = var_temp word_ty in
      let+ c = mov_const w tmp ~eff in
      {value = tmp; eff = c} in
    let* o, a, b = match a.value, b.value with
      | Const w, Const _ ->
        let+ a = move w a.eff in
        o, a, b
      | Const w, _ when not (commutative o || can_be_swapped o) ->
        let+ a = move w a.eff in
        o, a, b
      | Const _, _ when not @@ can_be_swapped o -> !!(o, b, a)
      | _ -> !!(o, a, b) in
    let* o = sel_binop o ~branch in
    o a b
  | UnOp (o, a) ->
    let default () =
      let* a = exp a in
      let* o = sel_unop o in
      o a in
    begin match o, Type.infer a with
      | _ , Error e ->
        fail @@ Format.asprintf
          "select_exp: Type.infer failed: %a"
          Type.Error.pp e
      | NOT, Ok (Imm n) ->
        if n = 1 || Option.is_some branch then
          (* Lazy way to compute the negation of that boolean. *)
          let identity = Bil.(BinOp (EQ, a, Int (Word.zero 32))) in
          select_exp identity ~branch ~lhs:None
        else default ()
      | _ -> default ()
    end
  | Var v ->
    begin match Var.typ v with
      | Imm 1 when Option.is_some branch ->
        (* Lazy way to compute the boolean. *)
        let v = var v in
        let c = const @@ Word.zero 32 in
        let* o = sel_binop NEQ ~branch in
        o v c
      | _ when Option.is_some branch ->
        fail "select_exp: Ill-typed variable in the condition \
              of a branch"
      | Imm _ -> !!(var v)
      | Mem _ -> !!(mem v)
      | Unk -> 
        fail @@ Format.asprintf
          "select_exp: encountered variable %a of \
           unknown type" Var.pp v
    end
  | Int w -> !!(const w)
  | Cast (UNSIGNED, _, e) -> select_exp e ~branch ~lhs
  | Cast (SIGNED, _, _) -> fail @@ "select_exp: SIGNED cast is unsupported"
  | Let (_, _, _) -> fail @@ "select_exp: Let is unsupported"
  | Unknown (_, _) -> fail @@ "select_exp: Unknown is unsupported"
  | Ite (_, _, _) -> fail @@ "select_exp: Ite is unsupported"
  | Extract (_, _, _) -> fail @@ "select_exp: Extract is unsupported"
  | Concat (_, _) -> fail @@ "select_exp: Concat is unsupported"
  | _ -> assert false

and select_def (def : def term) : eff KB.t =
  let lhs = Def.lhs def in
  let rhs = Def.rhs def in
  match Var.typ lhs with
  | Imm _ | Unk ->
    let* rhs = select_exp rhs ~lhs:(Some lhs) in
    let lhs = Ir.Operand.Var (Ir.Opvar.create lhs) in
    mov lhs rhs
  | Mem _ ->
    let lhs_mem = Ir.Operand.Void (Ir.Opvar.create lhs) in
    (* We don't need to pass the lhs for mem assign, since
       none of the patterns we match against will apply here. *)
    let* rhs = select_exp rhs in
    mov lhs_mem rhs

and select_jmp
    (jmp : jmp term)
    (call_params : Ir.Operand.t list) : eff KB.t =
  let cond = Jmp.cond jmp in
  let is_call = Helpers.is_call jmp in
  Utils.get_dsts jmp >>= function
  | None -> fail @@ Format.asprintf "Unexpected branch: %a" Jmp.pp jmp
  | Some {Utils.dst; ret} ->
    let* eff = match cond with
      | Int w when Word.(w <> b0) ->
        (* Unconditional branch. If cond is zero, this should
           have been optimized away. *)
        goto dst call_params ~is_call
      | _ ->
        (* Conditional branch. *)
        let* () =
          if is_call then
            fail @@ Format.asprintf
              "Unsupported conditional call: %a"
              Jmp.pp jmp
          else !!() in
        let branch = Some (Branch.create dst) in
        let+ {eff; _} = select_exp cond ~branch in
        eff in
    (* If this was a call, then insert the destination we
       should return to. This often will get optimized away
       if we're returning to the immediate next block. *)
    match ret with
    | None -> !!eff
    | Some ret ->
      let+ ctrl = goto ret [] in
      ctrl @. eff

and select_stmt (call_params : Ir.Operand.t list) (s : Blk.elt) : eff KB.t =
  match s with
  | `Def def -> select_def def
  | `Jmp jmp -> select_jmp jmp call_params
  | `Phi _ -> !!empty_eff

and select_elts
    (call_params : Ir.Operand.t list)
    (elts : Blk.elt list) : eff KB.t =
  match elts with
  | [] -> KB.return empty_eff
  (* We only select 1 instruction at a time for now *)
  | s :: ss ->
    let* s = select_stmt call_params s in
    let+ ss = select_elts call_params ss in
    ss @. s

and select_blk (b : blk term) ~(param_info : Param.info) : eff KB.t =
  let+ {data; ctrl; ir} =
    Blk.elts b |> Seq.to_list |> List.filter ~f:(function
        | `Def d -> not @@ Tid.Set.mem param_info.ignored @@ Term.tid d
        | _ -> true) |> select_elts param_info.ops in
  let new_blk =
    Term.tid b |> Ir.Block.create_simple
      ~data:(List.rev data)
      ~ctrl:(List.rev ctrl) in
  {empty_eff with ir = Ir.add new_blk ir}

let rec select_blks ~(params : Param.t) : blk term list -> eff KB.t = function
  | [] -> !!empty_eff
  | blk :: rest ->
    let param_info =
      Term.tid blk |> Map.find params |>
      Option.value ~default:Param.empty_info in
    let* blk = select_blk blk ~param_info in
    let+ rest = select_blks rest ~params in
    blk @. rest

let select (sub : sub term) : Ir.t KB.t =
  let blks = Term.enum blk_t sub |> Seq.to_list in
  let params = Param.collect sub in
  let+ eff = select_blks blks ~params in
  eff.ir
