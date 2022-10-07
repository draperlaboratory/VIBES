open Core
open Bap.Std
open Bap_core_theory

module T = Theory
module Tags = Vibes_bir.Tags
module Ir = Vibes_ir.Types
module Helpers = Vibes_bir.Helpers
module Linear = Vibes_linear_ssa.Utils
module Naming = Vibes_higher_vars.Substituter.Naming
module Ops = Arm_ops
module Pre = Types.Preassign
module Param = Types.Call_params

open KB.Syntax
open Types.Sel

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

let is_stack_pointer (v : var) : bool = match Pre.reg_name v with
  | Some "SP" -> true
  | _ -> false

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
    let op = Ir.Operation.create_simple c tmp [dst; flg] in
    op, tmp

end

(* Check if the constant is greater than 255, in which case we want 
   movw rather than mov. If it's greater than 65535, then we will use
   the ldr pseudo instruction. The assembler will store the constant
   in a literal pool at the end of our patch, and it will access this
   constant with a PC-relative load.

   We should avoid using this pseudo-instruction if it's a negative
   number between -1 and -257. The assembler will turn it into a mvn
   instruction, but we can just do it here. This way, the ldrs that
   appear will be a signpost for us that the assembler will insert
   a constant pool (which is important for us to know in the patcher).
*)
let mov_const (c : word) ~(is_thumb : bool) : Ir.opcode * word =
  match Word.to_int_exn c with
  | n when n <= 0xFF -> Ops.mov is_thumb, c
  | n when n <= 0xFFFF -> Ops.movw, c
  | n when n >= 0xFFFF_FEFF && n <= 0xFFFF_FFFF -> Ops.mvn, Word.lnot c
  | _ -> Ops.ldr, c

let is_movcc (o : Ir.Operation.t) : bool =
  List.exists o.opcodes ~f:(fun n ->
      String.is_prefix n ~prefix:"mov" &&
      let n = String.drop_prefix n 3 in
      Option.is_some @@ Ops.Cond.of_string n)

let mov (l : Ir.Operand.t) (r : pure) ~(is_thumb : bool) : eff KB.t =
  match l, r.value with
  | Var _, Var _
    when not (
        List.is_empty r.eff.data ||
        (* The optimization below should not be performed on conditional
           instructions. *)
        is_movcc @@ List.hd_exn r.eff.data) -> begin
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
    let c = Ops.mov is_thumb in
    let mov = Ir.Operation.create_simple c l [r.value] in
    !!(instr mov r.eff)
  | Var _, Const w ->
    let c, w = mov_const w ~is_thumb in
    let mov = Ir.Operation.create_simple c l [Const w] in
    !!(instr mov r.eff)
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

let uop
    (o : Ir.opcode)
    (ty : typ)
    (arg : pure)
    ~(is_thumb : bool) : pure KB.t =
  let* res = var_temp ty in
  match arg.value with
  | Const w ->
    let+ tmp = var_temp word_ty in
    let c, w = mov_const w ~is_thumb in
    let mov = Ir.Operation.create_simple c tmp [Const w] in
    let op = Ir.Operation.create_simple o res [tmp] in
    {value = res; eff = instr op (instr mov arg.eff)}
  | _ ->
    let op = Ir.Operation.create_simple o res [arg.value] in
    !!{value = res; eff = instr op arg.eff}

let binop
    (o : Ir.opcode)
    (ty : typ)
    (l : pure)
    (r : pure)
    ~(is_thumb : bool) : pure KB.t =
  let* res = var_temp ty in
  match r.value with
  | Const w when Word.to_int_exn w > 0xFFF ->
    (* For binops that allow constant operands, the limit seems
       to be 12 bits according to the manual. *)
    let+ tmp = var_temp word_ty in
    let c, w = mov_const w ~is_thumb in
    let mov = Ir.Operation.create_simple c tmp [Const w] in
    let op = Ir.Operation.create_simple o res [l.value; tmp] in
    let eff = l.eff @. r.eff in
    {value = res; eff = instr op (instr mov eff)}
  | _ ->
    let op = Ir.Operation.create_simple o res [l.value; r.value] in
    let eff = l.eff @. r.eff in
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

let quadop
    (o : Ir.opcode)
    (ty : typ)
    (w : pure)
    (x : pure)
    (y : pure)
    (z : pure) : pure KB.t =
  let+ res = var_temp ty in
  let ops = [w.value; x.value; y.value; z.value] in
  let op = Ir.Operation.create_simple o res ops in
  let eff = w.eff @. x.eff @. y.eff @. z.eff in
  {value = res; eff = instr op eff}

let add (l : pure) (r : pure) ~(is_thumb : bool) : pure KB.t =
  binop Ops.(add is_thumb) word_ty l r ~is_thumb

let neg (x : pure) ~(is_thumb : bool) : pure KB.t =
  uop Ops.neg word_ty x ~is_thumb

let lognot (x : pure) ~(is_thumb : bool) : pure KB.t =
  uop Ops.mvn word_ty x ~is_thumb

let mul (l : pure) (r : pure) ~(is_thumb : bool) : pure KB.t =
  binop Ops.mul word_ty l r ~is_thumb

let sub (l : pure) (r : pure) ~(is_thumb : bool) : pure KB.t =
  binop Ops.(sub is_thumb) word_ty l r ~is_thumb

let lsl_ (l : pure) (r : pure) ~(is_thumb : bool) : pure KB.t =
  binop Ops.lsl_ word_ty l r ~is_thumb

let lsr_ (l : pure) (r : pure) ~(is_thumb : bool) : pure KB.t =
  binop Ops.lsr_ word_ty l r ~is_thumb

let asr_ (l : pure) (r : pure) ~(is_thumb : bool) : pure KB.t =
  binop Ops.asr_ word_ty l r ~is_thumb

let ldr_op (bits : int) : Ir.opcode KB.t =
  if bits = 32 then !!Ops.ldr
  else if bits = 16 then !!Ops.ldrh
  else if bits = 8 then !!Ops.ldrb
  else fail @@ Format.sprintf
      "ldr_op: loading a bit-width that is not \
       8, 16, or 32 (got %d)" bits

let ldr
    (bits : int)
    (mem : pure)
    (loc : pure)
    ~(is_thumb : bool) : pure KB.t =
  let* ldr = ldr_op bits in
  binop ldr word_ty mem loc ~is_thumb

let str_op (bits : int) : Ir.opcode KB.t =
  if bits = 32 then !!Ops.str
  else if bits = 16 then !!Ops.strh
  else if bits = 8 then !!Ops.strb
  else fail @@ Format.sprintf
      "str_op: storing a bit-width that is not \
       8, 16, or 32 (got %d)" bits

let str
    (bits : int)
    (mem : pure)
    (value : pure)
    (loc : pure)
    ~(is_thumb : bool) : pure KB.t =
  let* str = str_op bits in
  let* res = void_temp mem_ty in
  let+ ops = match value.value with
    | Var _ ->
      let ops = [mem.value; value.value; loc.value] in
      !![Ir.Operation.create_simple str res ops]
    | Const w ->
      let+ tmp = var_temp word_ty in
      let ops = [mem.value; tmp; loc.value] in
      let c, w = mov_const w ~is_thumb in
      let mov = Ir.Operation.create_simple c tmp [Const w] in
      let op = Ir.Operation.create_simple str res ops in
      [op; mov]
    | _ -> fail @@ Format.asprintf
        "str: unsupported `value` operand %a"
        Ir.Operand.pp value.value in
  let eff = loc.eff @. value.eff @. mem.eff in
  {value = res; eff = List.fold_right ops ~init:eff ~f:instr}

(* Special case of the `str` instruction where the address that we are
   storing to is of the shape `base + off` (e.g. `str R0, [R1, #8]`). *)
let str_base_off
    (mem : pure)
    (value : pure)
    (base : var)
    (off : word)
    ~(is_thumb : bool) : pure KB.t =
  let* res = void_temp mem_ty in
  let base = Ir.Operand.Var (Ir.Opvar.create base) in
  let off = Ir.Operand.Const off in
  let+ ops = match value.value with
    | Var _ ->
      let ops = [mem.value; value.value; base; off] in
      !![Ir.Operation.create_simple Ops.str res ops]
    | Const w ->
      let+ tmp = var_temp word_ty in
      let ops = [mem.value; tmp; base; off] in
      let c, w = mov_const w ~is_thumb in
      let mov = Ir.Operation.create_simple c tmp [Const w] in
      let op = Ir.Operation.create_simple Ops.str res ops in
      [op; mov]
    | _ -> fail @@ Format.asprintf
        "str_base_off: unsupported `value` operand %a"
        Ir.Operand.pp value.value in
  let eff = value.eff @. mem.eff in
  {value = res; eff = List.fold_right ops ~init:eff ~f:instr}

let logand (a : pure) (b : pure) ~(is_thumb : bool) : pure KB.t =
  binop Ops.and_ word_ty a b ~is_thumb

let logor (a : pure) (b : pure) ~(is_thumb : bool) : pure KB.t =
  binop Ops.orr word_ty a b ~is_thumb

let xor (a : pure) (b : pure) ~(is_thumb : bool) : pure KB.t =
  binop Ops.eor word_ty a b ~is_thumb

(* Specialization of binops for generating comparisons. *)
let binop_cmp
    (cond : Ops.cond)
    (l : pure)
    (r : pure)
    ~(is_thumb : bool)
    ~(branch : Branch.t option) : pure KB.t =
  let* tmp_flag = void_temp bit_ty in
  let eff = l.eff @. r.eff in
  match branch with
  | Some generate ->
    (* The comparison is used by a branch instruction, so use the
       `generate` function to add it to the ctrl semantics. It should
       be the only one in the current block. *)
    let* () = match eff.ctrl with
      | [] -> !!()
      | _ ->
        fail "binop_cmp: encountered a branch with non-empty \
              ctrl semantics" in
    let cmp_ops = [l.value; r.value] in
    let cmp = Ir.Operation.create_simple Ops.cmp tmp_flag cmp_ops in
    let+ br, value = generate ~cnd:cond ~flg:tmp_flag in
    (* Keep in mind, `value` should be discarded. *)
    {value; eff = control br (instr cmp eff)}
  | None ->
    (* Store the result of the comparison in an intermediate destination. *)
    let* tmp1 = temp word_ty in
    let+ tmp2 = temp word_ty in
    (* These temps need to be unique, but VIBES IR also needs to know
       that they are congruent (i.e. they must map to the same register). *)
    let eff =
      let x = List.hd_exn tmp1.temps in
      let y = List.hd_exn tmp2.temps in
      let cong = Ir.{
          blks = [];
          congruences = Var.Map.of_alist_exn [
              x, Var.Set.singleton y;
              y, Var.Set.singleton x;
            ];
        } in
      {eff with ir = Ir.union eff.ir cong} in
    (* On Thumb we generate `movs` for the compact encoding, but this would
       clobber the flags (which we want to avoid). The pattern we generate 
       will assume the condition is true first, and then clear the result
       if it is false. *)
    let then_ = Ops.mov is_thumb in
    let else_ = Ops.movcc @@ Ops.Cond.opposite cond in
    let then_ =
      Ir.Operation.create_simple then_ (Var tmp1) [Const Word.(one 32)] in
    let cmp =
      let ops = [l.value; r.value; Var tmp1] in
      Ir.Operation.create_simple Ops.cmp tmp_flag ops in
    let else_ = Ir.Operation.create_simple else_ (Var tmp2) [
        Const Word.(zero 32);
        tmp_flag;
        Var tmp1
      ] in
    {value = Var tmp2; eff = instr else_ (instr cmp (instr then_ eff))}

let equals
    ~(is_thumb : bool)
    ~(branch : Branch.t option) : pure -> pure -> pure KB.t =
  binop_cmp EQ ~is_thumb ~branch

let not_equals
    ~(is_thumb : bool)
    ~(branch : Branch.t option) : pure -> pure -> pure KB.t =
  binop_cmp NE ~is_thumb ~branch

let less_than
    (l : pure)
    (r : pure)
    ~(is_thumb : bool)
    ~(branch : Branch.t option) : pure KB.t =
  match l.value with
  | Const _ -> binop_cmp HI r l ~is_thumb ~branch
  | _ -> binop_cmp LO l r ~is_thumb ~branch

let less_or_equal
    (l : pure)
    (r : pure)
    ~(is_thumb : bool)
    ~(branch : Branch.t option) : pure KB.t =
  match l.value with
  | Const _ -> binop_cmp HS r l ~is_thumb ~branch
  | _ -> binop_cmp LS l r ~is_thumb ~branch

let signed_less_than
    (l : pure)
    (r : pure)
    ~(is_thumb : bool)
    ~(branch : Branch.t option) : pure KB.t =
  match l.value with
  | Const _ -> binop_cmp GT r l ~is_thumb ~branch
  | _ -> binop_cmp LT l r ~is_thumb ~branch 

let signed_less_or_equal
    (l : pure)
    (r : pure)
    ~(is_thumb : bool)
    ~(branch : Branch.t option) : pure KB.t =
  match l.value with
  | Const _ -> binop_cmp GE r l ~is_thumb ~branch
  | _ -> binop_cmp LE l r ~is_thumb ~branch

(* Unconditional jump *)
let goto
    ?(is_call : bool = false)
    (tgt : Ir.Operand.t)
    (call_params : Ir.Operand.t list) : eff KB.t =
  let c = if is_call then Ops.bl () else Ops.b () in
  (* XXX: should we figure out how to better describe the effects of the call?
     Ideally, we should say that the call will clobber the memory and all
     caller-save registers. We could probably do this at the BIR level, at
     the return successor of each call site, where we make each effect
     explicit. *)
  let+ tmp_branch = void_temp bit_ty in
  let op = Ir.Operation.create_simple c tmp_branch (tgt :: call_params) in
  control op empty_eff

let sel_binop
    (o : binop)
    ~(is_thumb : bool)
    ~(branch : Branch.t option) : (pure -> pure -> pure KB.t) KB.t =
  match o with
  | PLUS -> !!(add ~is_thumb)
  | MINUS -> !!(sub ~is_thumb)
  | TIMES -> !!(mul ~is_thumb)
  | LSHIFT -> !!(lsl_ ~is_thumb)
  | RSHIFT -> !!(lsr_ ~is_thumb)
  | ARSHIFT ->  !!(asr_ ~is_thumb)
  | AND -> !!(logand ~is_thumb)
  | OR -> !!(logor ~is_thumb)
  | EQ -> !!(equals ~is_thumb ~branch)
  | NEQ -> !!(not_equals ~is_thumb ~branch)
  | LT -> !!(less_than ~is_thumb ~branch)
  | LE -> !!(less_or_equal ~is_thumb ~branch)
  | SLT -> !!(signed_less_than ~is_thumb ~branch)
  | SLE -> !!(signed_less_or_equal ~is_thumb ~branch)
  | XOR -> !!(xor ~is_thumb)
  | DIVIDE | SDIVIDE | MOD | SMOD -> fail @@ Format.sprintf
      "sel_binop: unsupported operation %s"
      (Bil.string_of_binop o)

let get_const (v : 'a Theory.Bitv.t Theory.value) : word option =
  match KB.Value.get Exp.slot v with
  | Int w -> Some w
  | _ -> None

let get_label (tid : tid) : Ir.Operand.t KB.t =
  let+ addr = KB.collect Theory.Label.addr tid in
  match addr with
  | None -> Ir.Operand.Label tid
  | Some addr ->
    let w = Bitvec.to_int addr |> Word.of_int ~width:32 in
    Ir.Operand.Offset w

type dsts = {
  dst : Ir.Operand.t;
  ret : Ir.Operand.t option;
}

let get_dsts (jmp : jmp term) : dsts option KB.t =
  let aux dst = match Jmp.resolve dst with
    | First dst -> get_label dst >>| Option.return
    | Second c -> KB.return @@ Option.map
        ~f:(fun w -> Ir.Operand.Offset w) (get_const c) in
  match Jmp.dst jmp, Jmp.alt jmp with
  | Some dst, None ->
    aux dst >>| begin function
      | Some dst -> Some {dst; ret = None}
      | None -> None
    end
  | Some dst, Some alt -> begin
      aux dst >>= function
      | None -> !!None
      | Some ret ->  aux alt >>| function
        | Some dst -> Some {dst; ret = Some ret}
        | None -> None
    end
  | _ -> !!None

let sel_unop (o : unop) ~(is_thumb : bool) : (pure -> pure KB.t) KB.t =
  match o with
  | NOT -> !!(lognot ~is_thumb)
  | NEG -> !!(neg ~is_thumb)

(* `lhs` is the left-hand side of the Def term that we are selecting from
   (if any). The selector can make more informed decisions with this info.
   Note that this only applies to the top-level expression, not intermediate
   operations generated from subexpressions.

   `branch` also applies to top-level expressions. We're going to use this
   when handling the condition for a branch instruction. This lets us
   generate better code and not have to write a specialized version of this
   function.

   NOTE: these parameters generally are NOT to be passed to subexpressions
   (e.g. recursive calls to this function). These calls are meant to handle
   intermediate operations (which would store their results in a temporary
   variable). If these arguments are applied to those recursive calls, then
   things could go very wrong.
*)
let rec select_exp
    ?(branch : Branch.t option = None)
    ?(lhs : var option = None)
    (e : exp)
    ~(is_thumb : bool) : pure KB.t =
  let exp = select_exp ~is_thumb ~branch:None ~lhs:None in
  let exp_binop_integer = select_exp_binop_integer ~is_thumb in
  match e with
  | Load (mem, BinOp (PLUS, a, BinOp (TIMES, Int s, b)), _, size)
    when Int.is_pow2 @@ Word.to_int_exn s ->
    let* mem = exp mem in
    let* ldr = ldr_op @@ Size.in_bits size in
    let* a = exp a in
    let* b = exp b in
    let width = Word.bitwidth s in
    let s =
      const @@
      Word.of_int ~width @@
      Int.ctz @@
      Word.to_int_exn s in
    quadop ldr word_ty mem a b s
  | Load (mem, BinOp (PLUS, a, Int w), _, size) ->
    let* mem = exp mem in
    let* ldr = ldr_op @@ Size.in_bits size in
    let* a = exp a in
    let w = const w in
    ternop ldr word_ty mem a w
  | Load (mem, BinOp (MINUS, a, Int w), _, size) ->
    let* mem = exp mem in
    let* ldr = ldr_op @@ Size.in_bits size in
    let* a = exp a in
    let w = const (Word.neg w) in
    ternop ldr word_ty mem a w
  | Load (mem, Int addr, _, size) ->
    let* mem = exp mem in
    let* tmp = var_temp word_ty in
    let c, w = mov_const addr ~is_thumb in
    let op = Ir.Operation.create_simple c tmp [Const w] in
    let a = {value = tmp; eff = instr op empty_eff} in
    let* ldr = ldr_op @@ Size.in_bits size in
    ternop ldr word_ty mem a @@ const (Word.zero 32)
  | Load (mem, loc, _, size) ->
    let* mem = exp mem in
    let* loc = exp loc in
    ldr (Size.in_bits size) mem loc ~is_thumb
  | Store (mem, BinOp (PLUS, Var a, Int w), value, _ , _size) ->
    let* mem = exp mem in
    let* value = exp value in
    str_base_off mem value a w ~is_thumb
  | Store (mem, BinOp (MINUS, Var a, Int w), value, _ , _size) ->
    let* mem = exp mem in
    let* value = exp value in
    str_base_off mem value a Word.(-w) ~is_thumb
  | Store (mem, Int addr, value, _, size) ->
    let* mem = exp mem in
    let* tmp = var_temp word_ty in
    let c, w = mov_const addr ~is_thumb in
    let op = Ir.Operation.create_simple c tmp [Const w] in
    let loc = {value = tmp; eff = instr op empty_eff} in
    let* value = exp value in
    str (Size.in_bits size) mem value loc ~is_thumb
  | Store (mem, loc, value, _, size) ->
    let* mem = exp mem in
    let* loc = exp loc in
    let* value = exp value in
    (* We have to swap the arguments here, since ARM likes the value
       first, and the location second *)
    str (Size.in_bits size) mem value loc ~is_thumb
  (* This should've been handled by the optimizer, but we will check anyway. *)
  | BinOp (PLUS, Int w, a) when Word.(w = zero 32) -> exp a
  | BinOp ((PLUS | MINUS), a, Int w) when Word.(w = zero 32) -> exp a
  (* FIXME: this is amost certainly wrong *)
  | BinOp (PLUS, a, b) when Exp.(a = b) ->
    let* a = exp a in
    lsl_ a ~is_thumb @@ const @@ Word.one 32
  (* Thumb 2 encoding allows adding an 8-bit immediate, when
     source and destination registers are the same. *)
  | BinOp ((PLUS | MINUS) as o, Var a, Int w)
  | BinOp ((PLUS | MINUS) as o, Int w, Var a)
    when Option.exists lhs ~f:(Linear.same a)
      && is_thumb && Word.to_int_exn w <= 0xFF ->
    let set_flags = not @@ is_stack_pointer a in
    let op = if Caml.(o = PLUS) then Ops.add else Ops.sub in
    binop (op set_flags) word_ty (var a) (const w) ~is_thumb
  (* Move the immediate operand to a temporary. *)
  | BinOp (TIMES as o, Int w, x) | BinOp (TIMES as o, x, Int w) ->
    let i = Word.to_int_exn w in
    (* Power of two can be simplified to a left shift. *)
    if Int.is_pow2 i then
      let zero = const (Word.zero 32) in
      let sh = Int.ctz i in
      (* Greater than 31 is not encodable, but that also just means we're
         shifting out every bit, so the result is zero. *)
      if sh > 31 then KB.return zero
      else
        let* x = exp x in
        lsl_ x ~is_thumb @@ const @@ Word.of_int sh ~width:32
    else exp_binop_integer o w x
  (* Immediate shift value must be within the range 1-32. *)
  | BinOp (ARSHIFT as o, x, Int w)
    when Word.(w < one 32 || w > of_int ~width:32 32) ->
    exp_binop_integer o w x ~swap:true
  (* Immediate shift value must be within the range 0-31. *)
  | BinOp ((LSHIFT | RSHIFT), _, Int w)
    when Word.(w > of_int ~width:32 31) -> KB.return @@ const @@ Word.zero 32
  (* Move the immediate operand to a temporary. *)
  | BinOp ((LSHIFT | RSHIFT | ARSHIFT) as o, Int w, x)
  | BinOp ((OR | AND | XOR) as o, Int w, x)
  | BinOp ((OR | AND | XOR) as o, x, Int w) -> exp_binop_integer o w x
  | BinOp (o, a, b) ->
    let* a = exp a in
    let* b = exp b in
    let* o = sel_binop o ~is_thumb ~branch in
    o a b
  | UnOp (o, a) -> begin
      let default () =
        let* a = exp a in
        let* o = sel_unop o ~is_thumb in
        o a in
      match o, Type.infer a with
      | _ , Error e ->
        fail @@ Format.asprintf
          "select_exp: Type.infer failed: %a"
          Type.Error.pp e
      | NOT, Ok (Imm n) ->
        if n = 1 || Option.is_some branch then
          (* Lazy way to compute the negation of that boolean. *)
          let identity = Bil.(BinOp (EQ, a, Int (Word.zero 32))) in
          select_exp identity ~branch ~is_thumb ~lhs:None
        else default ()
      | _ -> default ()
    end
  | Var v -> begin
      match Var.typ v with
      | Imm 1 when Option.is_some branch ->
        (* Lazy way to compute the boolean. *)
        let v = var v in
        let c = const @@ Word.zero 32 in
        let* o = sel_binop NEQ ~is_thumb ~branch in
        o v c
      | _ when Option.is_some branch ->
        fail "select_exp: Ill-typed variable in the condition \
              of a branch"
      | Imm _ -> KB.return @@ var v
      | Mem _ -> KB.return @@ mem v
      | Unk -> fail @@ Format.asprintf
          "select_exp: encountered variable %a of \
           unknown type" Var.pp v
    end
  | Cast (UNSIGNED, _, e) -> select_exp e ~is_thumb ~branch ~lhs
  | Cast (SIGNED, _, _) -> fail @@ "select_exp: SIGNED cast is unsupported"
  | Cast (LOW, _, _) -> fail @@ "select_exp: LOW Cast is unsupported"
  | Cast (HIGH, _, _) -> fail @@ "select_exp: HIGH cast is unsupported"
  | Int w -> !!(const w)
  | Let (_, _, _) -> fail @@ "select_exp: Let is unsupported"
  | Unknown (_, _) -> fail @@ "select_exp: Unknown is unsupported"
  | Ite (_, _, _) -> fail @@ "select_exp: Ite is unsupported"
  | Extract (_, _, _) -> fail @@ "select_exp: Extract is unsupported"
  | Concat (_, _) -> fail @@ "select_exp: Concat is unsupported"

(* Helper function for selecting instructions that correspond to binops, but
   whose operands must all be registers. Therefore, if one of the operands
   is an immediate value, it must be loaded into a register first using an
   intermediate operation.

   `swap` will swap the order of the operands `lhs` and `rhs`.
*)
and select_exp_binop_integer
    ?(swap : bool = false)
    (o : binop)
    (lhs : word)
    (rhs : exp)
    ~(is_thumb : bool) : pure KB.t =
  let* rhs = select_exp rhs ~is_thumb in
  let* o = sel_binop o ~is_thumb ~branch:None in
  let* tmp = var_temp word_ty in
  let c = Ops.mov is_thumb in
  let op = Ir.Operation.create_simple c tmp [Const lhs] in
  let lhs = {value = tmp; eff = instr op empty_eff} in
  if swap then o rhs lhs else o lhs rhs

and select_def
    (def : def term)
    ~(is_thumb : bool) : eff KB.t =
  let exp = select_exp ~is_thumb in
  let lhs = Def.lhs def in
  let rhs = Def.rhs def in
  let mov = mov ~is_thumb in
  match Var.typ lhs with
  | Imm _ | Unk ->
    let* rhs = exp rhs ~lhs:(Some lhs) in
    let lhs = Ir.Operand.Var (Ir.Opvar.create lhs) in
    mov lhs rhs
  | Mem _ ->
    let lhs_mem = Ir.Operand.Void (Ir.Opvar.create lhs) in
    (* We don't need to pass the lhs for mem assign, since
       none of the patterns we match against will apply here. *)
    let* rhs = exp rhs in
    mov lhs_mem rhs

and select_jmp
    (jmp : jmp term)
    (call_params : Ir.Operand.t list)
    ~(is_thumb : bool) : eff KB.t =
  let exp = select_exp ~is_thumb in
  let cond = Jmp.cond jmp in
  let is_call = Helpers.is_call jmp in
  get_dsts jmp >>= function
  | None -> fail @@ Format.asprintf "Unexpected branch: %a" Jmp.pp jmp
  | Some {dst; ret} ->
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
        let+ {eff; _} = exp cond ~branch in
        eff in
    (* If this was a call, then insert the destination we
       should return to. This often will get optimized away
       if we're returning to the immediate next block. *)
    match ret with
    | None -> !!eff
    | Some ret ->
      let+ ctrl = goto ret [] in
      ctrl @. eff

and select_stmt
    (call_params : Ir.Operand.t list)
    (s : Blk.elt)
    ~(is_thumb : bool) : eff KB.t =
  match s with
  | `Def def -> select_def def ~is_thumb
  | `Jmp jmp -> select_jmp jmp call_params ~is_thumb
  | `Phi _ -> !!empty_eff

and select_elts
    (call_params : Ir.Operand.t list)
    (elts : Blk.elt list)
    ~(is_thumb : bool) : eff KB.t =
  match elts with
  | [] -> KB.return empty_eff
  (* We only select 1 instruction at a time for now *)
  | s :: ss ->
    let* s = select_stmt call_params s ~is_thumb in
    let+ ss = select_elts call_params ss ~is_thumb in
    ss @. s

and select_blk
    (b : blk term)
    ~(is_thumb : bool)
    ~(param_info : Param.info) : eff KB.t =
  let+ {data; ctrl; ir} =
    Blk.elts b |> Seq.to_list |> List.filter ~f:(function
        | `Def d -> not @@ Tid.Set.mem param_info.ignored @@ Term.tid d
        | _ -> true) |> select_elts param_info.ops ~is_thumb in
  let new_blk =
    Term.tid b |> Ir.Block.create_simple
      ~data:(List.rev data)
      ~ctrl:(List.rev ctrl) in
  {empty_eff with ir = Ir.add new_blk ir}

let rec select_blks
    (blks : blk term list)
    ~(is_thumb : bool)
    ~(params : Param.t) : eff KB.t =
  match blks with
  | [] -> !!empty_eff
  | blk :: rest ->
    let param_info =
      Term.tid blk |> Map.find params |>
      Option.value ~default:Param.empty_info in
    let* blk = select_blk blk ~is_thumb ~param_info in
    let+ rest = select_blks rest ~is_thumb ~params in
    blk @. rest

let select (sub : sub term) ~(is_thumb : bool) : Ir.t KB.t =
  let blks = Term.enum blk_t sub |> Seq.to_list in
  let params = Param.collect sub in
  let+ eff = select_blks blks ~is_thumb ~params in
  eff.ir
