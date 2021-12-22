open !Core_kernel
open Bap.Std
open Bap_core_theory
open KB.Let

module Err = Kb_error

(*-------------------------------------------------------
*
* Instruction selection works in the "standard" maximal munch
* manner: we match against a portion of the AST, call the selector
* recursively and generate opcodes.
*
*
*
*  The approach attempts to create a small DSL for opcodes and their
*  operands, ultimately "smart" constructors for [Ir.t] blocks, and
*  then use ordinary OCaml pattern matching, with DSL terms on the
*  right hand side.
*
*
* since BIR constructs may be at a higher-level than ARM ones, and be
* nested, we need to flatten the terms, as well as disentangle the
* different kinds of operations. We roughly follow the Core Theory
* division of data and control _effects_, and _pure_ operations which
* simply create data, we do _not_ distinguish a memory and non-memory
* operations, since we need to track dependencies between sequential
* memory reads and writes (though the variable name for memory doesn't
* really matter in the end).
*
* This results in 2 types:
*
* - [arm_eff] which contains the data required for the control and data
*   operations in the current block, as well as the other blocks
*   involved in the computation.
*
* - [arm_pure], which contains a variable or constant which represents
*   the value being computed, as well as the effects required to compute
*   that value (in the form of an [arm_eff] field.
*
* Every value is assumed to fit in a "standard" 32 bit register, so
* anything involving memory, or "double" width words have to be handled
* explicitly. It's currently an issue that we don't track bit-widths
* explicitly, since it may result in errors down the line, if we assume
* widths of registers or operands incorrectly. It sure simplifies the
* code though.
*
*
* There's a bunch of "preassign" operations throughout the module, since
* this allows the front-end to use special variable names, which we then
* know will be correctly allocated to specific registers, e.g. PC or
* SP. A little care must be taken depending on whether we're generating
* ARM proper or Thumb. There are probably some land mines here, since
* some instructions do not allow some registers, and this is not necessarily
* enforced by our constraints.
*
* We also provide the set of "allowed" general purpose registers, to be
* used by the minizinc back end when doing selection.
*
* The general approach to emitting instructions for BIL expressions is
* the following:
* 1. Recursively emit instructions for the sub-terms.
* 2. Generate a fresh temp to store the result.
* 3. Select the appropriate opcode, and move the result into the
*    generated temp.
* 4. Union the set of all effects that participated in building the
*    result.
* 5. Return the resulting [arm_pure] record.
*
* The opcodes are roughly just the ARM assembly mnemonic strings,
* they're created in the specific [Ops] module. Actually building
* the opcodes with their arguments (the aforementioned DSL) is done in
* the [Arm_ops] module.
*
* The selection is done by a series of pattern matching code, namely the
* [select_FOO] functions, where [FOO] is one of [exp], [stmt] or [blk].
*
* The pretty printer takes the resulting Vibes IR block, and is
* relatively straightforward except for various idiosyncrasies of the
* ARM assembly syntax:
*
* Roughly the only real issue is adding square brackets at the
* appropriate places. For example, the [str] operation can support
* (among many others!) the syntaxes [str r0, [r1, #42]], [str r0, [r1]]
* etc.
*
* We do that by marking operands by one of four tags (of a type called
* [bracket]): [Open], [Close], [Neither] or [Both]. These tags mark
* whether to open a square bracket, close one, enclose the operand or
* neither. Roughly we just create a parallel list of the same length as operands.
*
* We do this in kind of a hacky way, but I'm not sure how to do this
* better.
*
* Pretty printing is also the time when we resolve offsets: we do the
* arithmetic required to make them correct relative the the original
* binary, even though they may be in the patch location. See
* [arm_operand_pretty] for details.
*
*
* Finally, we create a peephole optimization which, *after register
* allocation*, deletes all instructions of the form [mov ri ri] (or of
* the form [add ri #0]). This is the only point when this can be done,
* since before register allocation we do not know if the [mov] will be
* of that form.
*
* Sadly there is no way to have an explicit [nop] generated at the moment.
*
*
*
*
*---------------------------------------------------------*)



type arm_eff = {
  (* These are the move/load/store operations in the current block *)
  current_data : Ir.operation list;
  (* These are the jump/goto operations in the current block *)
  current_ctrl : Ir.operation list;
  (* This is the list of blocks which encode the semantics of the
     operation, except those contained in [current_data] and
     [current_ctrl] *)
  other_blks : Ir.t}
[@@deriving compare, equal, sexp]

let empty_eff = {current_data = []; current_ctrl = []; other_blks = Ir.empty}

type arm_pure = {op_val : Ir.operand; op_eff : arm_eff}
[@@deriving compare, equal, sexp]

(* The default type for memory words *)
let word_ty = Bil.Types.Imm 32
let bit_ty = Bil.Types.Imm 1
let mem_ty = Bil.Types.Mem (`r32, `r8)

let is_arm (lang : Theory.language) : bool KB.t =
  if Theory.Language.is_unknown lang
  then Err.(fail Unknown_encoding)
  else
    KB.return @@
    String.is_substring ~substring:"arm" @@
    Theory.Language.to_string lang

let is_thumb (lang : Theory.language) : bool KB.t =
  if Theory.Language.is_unknown lang
  then Err.(fail Unknown_encoding)
  else
    KB.return @@
    String.is_substring ~substring:"thumb" @@
    Theory.Language.to_string lang
 
let is_arm_or_thumb (lang : Theory.language) : bool KB.t =
  let+ arm = is_arm lang and+ thumb = is_thumb lang in
  arm || thumb
 
(* FIXME: this feels very redundant: we should just leave the
   responsibility for this in ir.ml or minizinc.ml *)
let regs (tgt : Theory.target) (_ : Theory.language) =
  let bap_regs =
    Theory.Target.regs tgt |> Set.map ~f:Var.reify (module Var)
  in
  let pc = Var.create ~is_virtual:false ~fresh:false "PC" word_ty in
  Var.Set.add bap_regs pc

let gpr (tgt : Theory.target) (lang : Theory.language) : Var.Set.t KB.t =
  let+ thumb = is_thumb lang in
  let roles = [Theory.Role.Register.general] in
  let roles =
    if thumb
    then Theory.Role.read ~package:"arm" "thumb"::roles
    else roles
  in
  let exclude = [
    Theory.Role.Register.stack_pointer;
    Theory.Role.Register.frame_pointer
  ] in
  let maybe_reify v =
    let v = Var.reify v in
    let name = Var.name v in
    if String.(is_prefix name ~prefix:"R") &&
       not (thumb && String.(equal name "R7"))
    then Some v
    else None
  in
  Theory.Target.regs ~exclude:exclude ~roles:roles tgt |>
  Set.filter_map ~f:(maybe_reify) (module Var)

let reg_name (v : var) : string =
  let name = Var.name @@ Ir.drop_prefix v in
  try Linear_ssa.orig_name name with _ -> name

let preassign_var
    (is_thumb : bool)
    (pre : var option)
    (v : var)
  : var option =
  let name = reg_name v in
  let is_virtual = false and fresh = false in
  match name with
  | "FP" ->
    (* We assign R11 as the pre-assigned FP register on ARM, and R7
       for Thumb, keeping in line with the ABI (as far as i can
       tell).  *)
    if is_thumb
    then Some (Var.create ~is_virtual ~fresh "R7" (Var.typ v))
    else Some (Var.create ~is_virtual ~fresh "R11" (Var.typ v))
    (* FIXME: preassign all non-virtual variables? (or just the ones in tgt.regs?) *)
  | "PC" -> Some (Var.create ~is_virtual ~fresh "PC" (Var.typ v))
  | "SP" -> Some (Var.create ~is_virtual ~fresh "SP" (Var.typ v))
  | _ ->
    if (String.length name = 2
        && Char.(name.[0] = 'R')
        && Char.is_digit name.[1])
    || (String.length name = 3
        && Char.(name.[0] = 'R')
        && Char.is_digit name.[1]
        && Char.is_digit name.[2])
    then Some (Var.create ~is_virtual ~fresh name (Var.typ v))
    else pre
      
let preassign
    (tgt : Theory.target)
    (ir : Ir.t)
    ~(is_thumb : bool)
  : Ir.t =
  let ir = Ir.preassign tgt ir in
  let ir = Ir.map_op_vars ir
      ~f:(fun v ->
          {v with
           pre_assign =
             List.hd_exn v.temps |>
             preassign_var is_thumb v.pre_assign})
  in ir

(* Appends the effects of s2 to those of s1, respecting the order if they
   are not in seperate blocks. *)
let (@.) (s1 : arm_eff) (s2 : arm_eff) : arm_eff =
  let { current_data = data1; current_ctrl = ctrl1; other_blks = blks1} = s1 in
  let { current_data = data2; current_ctrl = ctrl2; other_blks = blks2} = s2 in
  {
    current_data = data1 @ data2;
    current_ctrl = ctrl1 @ ctrl2;
    other_blks = Ir.union blks1 blks2
  }


module ARM_ops = struct

  module Ops = struct

    include Ir.Opcode

    let op s = create ~arch:"arm" s

    let mov = op "mov"
    let movw = op "movw"
    let movt = op "movt"
    let add = op "add"
    let addw = op "addw"
    let mul = op "mul"
    let sub = op "sub"
    let lsl_ = op "lsl"
    let lsr_ = op "lsr"
    let asr_ = op "asr"
    let and_ = op "and"
    let orr = op "orr"
    let eor = op "eor"
    let ldr = op "ldr"
    let ldrh = op "ldrh"
    let ldrb = op "ldrb"
    let str = op "str"
    let cmp = op "cmp"
    let sdiv = op "sdiv"
    let udiv = op "udiv"
    let b = op "b"
    let bl = op "bl"
    let beq = op "beq"
    let bleq = op "bleq"
    let bne = op "bne"
    let blne = op "blne"
    let _ble = op "ble"
    let _blt = op "blt"
    let _bx = op "bx"

  end

  let create_temp ty =
    Var.create ~is_virtual:true ~fresh:true "tmp" ty |>
    Ir.simple_var

  (* defaults to data instructions, since they are way more common *)
  let instr i sem =
    {sem with current_data = i::sem.current_data}

  (* Create a control statement. *)
  let control j sem =
    {sem with current_ctrl = j::sem.current_ctrl}

  (* Some cowboy type checking here, to check which kind of mov to
     use. Currently doesn't work if variables are instantiated
     with spilled registers! Can be fixed by having seperate Variable
     constructors for spilled and non-spilled registers. *)
  let arm_mov (arg1 : Ir.operand) (arg2 : arm_pure) : arm_eff KB.t =
    let {op_val = arg2_var; op_eff = arg2_sem} = arg2 in
    match arg1, arg2_var with
    | Ir.Var _, Ir.Var _ when not @@ List.is_empty arg2_sem.current_data ->
      (* FIXME: absolute hack! if we have vars here, we can assume
         that the last operation assigned to a temporary, and we can
         just replace that temporary with the known destination, and
         return that as the effect. *)
      begin
        match arg2_sem.current_data with
        | [] -> assert false (* excluded by the guard above *)
        | op :: ops ->
          let op = {op with Ir.lhs = [arg1]} in
          KB.return {arg2_sem with current_data = op :: ops}
      end
    | Ir.Var _, Ir.Var _
    | Ir.Var _, Ir.Const _ ->
      (* Check if the second arg is a constant greater than 255, in which case
          we want movw rather than mov. If it's greater than 65535, then it
         needs to be split into two operations (movw/movt). *)
      begin
        match arg2_var with
        | Ir.Const w when Word.to_int_exn w <= 0xFF ->
          let mov = Ir.simple_op Ops.mov arg1 [arg2_var] in
          KB.return @@ instr mov arg2_sem
        | Ir.Const w when Word.to_int_exn w <= 0xFFFF ->
          let mov = Ir.simple_op Ops.movw arg1 [arg2_var] in
          KB.return @@ instr mov arg2_sem
        | Ir.Const w ->
          Err.(fail @@ Other (
              sprintf "arm_mov: too large constant %s" @@ Word.to_string w))
        | _ ->
          let mov = Ir.simple_op Ops.mov arg1 [arg2_var] in
          KB.return @@ instr mov arg2_sem
      end
    | Ir.Void _, Ir.Void _ when not @@ List.is_empty arg2_sem.current_data ->
      (* Same hack as above, but with void operands. *)
      begin
        match arg2_sem.current_data with
        | [] -> assert false
        | op :: ops ->
          let op = {op with Ir.lhs = [arg1]} in
          KB.return {arg2_sem with current_data = op :: ops}
      end
    | _ ->
      Err.(fail @@ Other (
          sprintf "arm_mov: unexpected arguments (%s, %s)"
            (Ir.pretty_operand arg1)
            (Ir.pretty_operand arg2_var)))

  let ( := ) x y = arm_mov x y

  let var v = {op_val = Ir.Var (Ir.simple_var v); op_eff = empty_eff}
  let mem v = {op_val = Ir.Void (Ir.simple_var v); op_eff = empty_eff}

  let const c = {op_val = Ir.Const c; op_eff = empty_eff}

  let _uop o ty arg =
    let res = create_temp ty in
    let {op_val = arg_val; op_eff = arg_sem} = arg in
    let op = Ir.simple_op o (Ir.Var res) [arg_val] in
    let sem = {arg_sem with current_data = op::arg_sem.current_data} in
    {op_val = Ir.Var res; op_eff = sem}

  let binop o ty arg1 arg2 =
    let res = create_temp ty in
    let {op_val = arg1_val; op_eff = arg1_sem} = arg1 in
    let {op_val = arg2_val; op_eff = arg2_sem} = arg2 in
    let op = Ir.simple_op o (Ir.Var res) [arg1_val; arg2_val] in
    let sem = arg1_sem @. arg2_sem in
    let sem = {sem with current_data = op::sem.current_data} in
    {op_val = Ir.Var res; op_eff = sem}

  let ternop o ty arg1 arg2 arg3 =
    let res = create_temp ty in
    let {op_val = arg1_val; op_eff = arg1_sem} = arg1 in
    let {op_val = arg2_val; op_eff = arg2_sem} = arg2 in
    let {op_val = arg3_val; op_eff = arg3_sem} = arg3 in
    let op = Ir.simple_op o (Ir.Var res) [arg1_val; arg2_val; arg3_val] in
    let sem = arg1_sem @. arg2_sem @. arg3_sem in
    let sem = {sem with current_data = op::sem.current_data} in
    {op_val = Ir.Var res; op_eff = sem}

  let (+) (arg1 : arm_pure) (arg2 : arm_pure) : arm_pure KB.t =
    (* If constant arg2 is wider than 3 bits -> emit addw *)
    let { op_val; _ } = arg2 in
    let fits_in_3 w =
      let open Word in
      let width = bitwidth w in
      of_int ~width 0 <= w &&
      w <= of_int ~width 7
    in
    match op_val with
    | Const w when not (fits_in_3 w) ->
      KB.return @@ binop Ops.addw word_ty arg1 arg2
    | _ ->
      KB.return @@ binop Ops.add word_ty arg1 arg2

  let ( * ) (arg1 : arm_pure) (arg2 : arm_pure) : arm_pure KB.t =
    KB.return @@ binop Ops.mul word_ty arg1 arg2

  let (-) (arg1 : arm_pure) (arg2 : arm_pure) : arm_pure KB.t =
    KB.return @@ binop Ops.sub word_ty arg1 arg2

  let (/) (arg1 : arm_pure) (arg2 : arm_pure) : arm_pure KB.t =
    KB.return @@ binop Ops.sdiv word_ty arg1 arg2

  let udiv (arg1 : arm_pure) (arg2 : arm_pure) : arm_pure KB.t =
    KB.return @@ binop Ops.udiv word_ty arg1 arg2

  let shl (_signed : arm_pure)
      (arg1 : arm_pure) (arg2 : arm_pure) : arm_pure KB.t =
    KB.return @@ binop Ops.lsl_ word_ty arg1 arg2

  let shr (signed : arm_pure)
      (arg1 : arm_pure) (arg2 : arm_pure) : arm_pure KB.t =
    let+ b =
      match signed.op_val with
      | Ir.Const w -> KB.return ((Word.to_int_exn w) <> 0)
      (* FIXME: Not sure what to do here; generally shifts are done by
         constant amounts, and at any rate it requires a bit of work
         to implement in ARM. Most likely the right thing to do is
         fail gracefully.  *)
      | _ -> Err.(fail @@ Other (
          sprintf "Arm_gen.shr: arg2 non-constant: %s"
            (Ir.pretty_operand signed.op_val)))
    in
    if b then
      binop Ops.asr_ word_ty arg1 arg2
    else
      binop Ops.lsr_ word_ty arg1 arg2

  let ldr_op (bits : int) : Ir.opcode KB.t =
    if bits = 32 then
      KB.return Ops.ldr
    else if bits = 16 then
      KB.return Ops.ldrh
    else if bits = 8 then
      KB.return Ops.ldrb
    else
      Err.(fail @@ Other (
          sprintf "Arm_selector.ldr: Loading a bit-width that is not \
                   8, 16 or 32 (got %d)!" bits))

  let ldr (bits : int) (mem : arm_pure) (loc : arm_pure) : arm_pure KB.t =
    let+ ldr = ldr_op bits in
    (* Update the semantics of loc with those of mem *)
    binop ldr word_ty mem loc

  let str (mem : arm_pure) (value : arm_pure) (loc : arm_pure) : arm_pure KB.t =
    let res = create_temp mem_ty in
    let lhs = Ir.Void (create_temp mem_ty) in
    let {op_val = loc_val; op_eff = loc_sem} = loc in
    let {op_val = value_val; op_eff = value_sem} = value in
    let {op_val = mem_val; op_eff = mem_sem} = mem in
    let+ ops = match value_val with
      | Var _ ->
        let op = Ir.simple_op Ops.str lhs [mem_val; value_val; loc_val] in
        KB.return [op]
      | Const _ ->
        let tmp = Ir.Var (create_temp word_ty) in
        let mov = Ir.simple_op Ops.mov tmp [value_val] in
        let op = Ir.simple_op Ops.str lhs [mem_val; tmp; loc_val] in
        KB.return [op; mov]
      | _ ->
        Err.(fail @@ Other (
            sprintf "str: unsupported `value` operand %s" @@
            Ir.pretty_operand value_val)) in
    let sem = loc_sem @. value_sem @. mem_sem in
    let sem = {sem with current_data = ops @ sem.current_data} in
    {op_val = Void res; op_eff = sem}

  (* Help take advantage of the loc shape `reg + imm` *)
  let str_base_off (mem : arm_pure) (value : arm_pure)
      (base : var) (off : word) : arm_pure KB.t =
    let res = create_temp mem_ty in
    let lhs = Ir.Void (create_temp mem_ty) in
    let {op_val = value_val; op_eff = value_sem} = value in
    let {op_val = mem_val; op_eff = mem_sem} = mem in
    let base = Ir.Var (Ir.simple_var base) in
    let off = Ir.Const off in
    let+ ops = match value_val with
      | Var _ ->
        let op = Ir.simple_op Ops.str lhs [mem_val; value_val; base; off] in
        KB.return [op]
      | Const _ ->
        let tmp = Ir.Var (create_temp word_ty) in
        let mov = Ir.simple_op Ops.mov tmp [value_val] in
        let op = Ir.simple_op Ops.str lhs [mem_val; tmp; base; off] in
        KB.return [op; mov]
      | _ ->
        Err.(fail @@ Other (
            sprintf "str_base_off: unsupported `value` operand %s" @@
            Ir.pretty_operand value_val)) in
    let sem = value_sem @. mem_sem in
    let sem = {sem with current_data = ops @ sem.current_data} in
    {op_val = Void res; op_eff = sem}
  
  let (&&) (a : arm_pure) (b : arm_pure) : arm_pure KB.t =
    KB.return @@ binop Ops.and_ word_ty a b

  let (||) (a : arm_pure) (b : arm_pure) : arm_pure KB.t =
    KB.return @@ binop Ops.orr word_ty a b

  let xor (a : arm_pure) (b : arm_pure) : arm_pure KB.t =
    KB.return @@ binop Ops.eor word_ty a b

  let equals (arg1 : arm_pure) (arg2 : arm_pure) : arm_pure KB.t =
    KB.return @@ binop Ops.sub word_ty arg1 arg2

  (* Intuitively we want to generate:

     ..cond.. //fills variable [cond_val]
     CMP cond_val 0
     BEQ tgt // or BLEQ if is_call is true
  *)
  (* Generally, boolean operations will be handled by a normal (word
     size) value, with value 0 if false and non-zero if true. It will
     be the job of branching operations to call [cmp ? ?] and check
     the appropriate flags. *)
  let br ?(neg = false) ?is_call:(is_call=false) cond tgt _lang =
    let opcode =
      if is_call then
        if neg then Ops.blne else Ops.bleq
      else
        if neg then Ops.bne else Ops.beq in
    let {op_val = cond_val; op_eff = cond_eff} = cond in
    (* the order of operations here is actually important! *)
    let tmp_flag, tmp_taken =
      create_temp bit_ty, create_temp bit_ty
    in
    let cmp = Ir.simple_op Ops.cmp (Void tmp_flag)
        [cond_val; Const (Word.of_int ~width:32 0)] in
    let beq = Ir.simple_op opcode (Void tmp_taken) [tgt] in
    let blks = cond_eff.other_blks in
    {
      current_data = cmp :: cond_eff.current_data;
      current_ctrl = [beq];
      other_blks = blks
    }

  (* Unconditional jump *)
  let goto ?is_call:(is_call=false) tgt arg_vars =
    let opcode = if is_call then Ops.bl else Ops.b in
    (* XXX: should we figure out how to better describe the effects of the call?
       Ideally, we should say that the call will clobber the memory and all
       caller-save registers. We could probably do this at the BIR level, at
       the return successor of each call site, where we make each effect
       explicit. *)
    let tmp_branch = create_temp bit_ty in
    (* The arg vars that were set before making this call should be
       marked as dependencies for the callee (since they are the parameters!).
       We don't want them to get clobbered. *)
    let arg_vars = List.map arg_vars ~f:(fun v ->
        match Var.typ v with
        | Mem _ -> Ir.Void (Ir.simple_var v)
        | _ -> Ir.Var (Ir.simple_var v)) in
    control (Ir.simple_op opcode (Void tmp_branch) ([tgt] @ arg_vars)) empty_eff


end

(* We assume that a block is always created *)
let ir (t : arm_eff) : Ir.t KB.t =
  if not Core_kernel.(
      List.is_empty t.current_data
      && List.is_empty t.current_ctrl) then
    Err.(fail @@ Other "Arm_selector.ir: expected empty data and ctrl")
  else
    let blks = t.other_blks in
    KB.return @@ Ir.add_in_vars blks


module ARM_Gen =
struct
  open ARM_ops

  let sel_binop (o : binop) : (arm_pure -> arm_pure -> arm_pure KB.t) KB.t =
    match o with
    | PLUS -> KB.return (+)
    | MINUS -> KB.return (-)
    | TIMES -> KB.return ( * )
    | DIVIDE -> KB.return udiv
    | SDIVIDE -> KB.return (/)
    | LSHIFT -> KB.return @@ shl (const (Word.zero 32))
    | RSHIFT -> KB.return @@ shr (const (Word.zero 32))
    | ARSHIFT ->  KB.return @@ shr (const (Word.one 32))
    | AND -> KB.return (&&)
    | OR -> KB.return (||)
    | EQ | NEQ -> KB.return equals
    | XOR -> KB.return xor
    | MOD
    | SMOD
    | LT
    | LE
    | SLT
    | SLE ->
      Err.(fail @@ Other (
          Format.sprintf "sel_binop: unsupported operation %s"
            (Bil.string_of_binop o)))

  let get_const (v : 'a Theory.Bitv.t Theory.value) : word option =
    let bil = KB.Value.get Exp.slot v in
    match bil with
    | Int w -> Some w
    | _ -> None

  let is_call (jmp : jmp term) : bool =
    match Jmp.kind jmp with
    | Call _ -> true
    | _ -> false

  let get_label (tid : tid) : Ir.operand KB.t =
    let+ addr = KB.collect Theory.Label.addr tid in
    match addr with
    | None -> Ir.Label tid
    | Some addr -> Ir.Offset (Bitvec.to_int addr |> Word.of_int ~width:32)

  let get_dst (jmp : jmp term) : Ir.operand option KB.t =
    let open KB.Syntax in
    match Jmp.dst jmp, Jmp.alt jmp with
    | Some dst, None ->
      begin
        match Jmp.resolve dst with
        | First dst -> get_label dst >>| Option.return
        | Second c -> KB.return @@ Option.map
            ~f:(fun w -> Ir.Offset w) (get_const c)
      end
    | Some _, Some dst ->
      begin
        match Jmp.resolve dst with
        | First dst -> get_label dst >>| Option.return
        | Second c -> KB.return @@ Option.map
            ~f:(fun w -> Ir.Offset w) (get_const c)
      end
    | _ -> KB.return @@ None

  let sel_unop (o : unop) : arm_pure -> arm_pure =
    match o with
    | NOT -> assert false
    | NEG -> assert false

  (* `lhs` is the left-hand side of the Def term that we are selecting from
     (if any). The selector can make more informed decisions with this info. *)
  let rec select_exp ?(lhs : var option = None)
      (lang : Theory.language) (e : Bil.exp) : arm_pure KB.t =
    let* thumb = is_thumb lang in
    match e with
    | Load (mem, BinOp (PLUS, a, Int w), _, size) ->
      let* mem = select_mem lang mem in
      let* ldr = ldr_op @@ Size.in_bits size in
      let+ a = select_exp lang a in
      let w = const w in
      ternop ldr word_ty mem a w
    | Load (mem, BinOp (MINUS, a, Int w), _, size) ->
      let* mem = select_mem lang mem in
      let* ldr = ldr_op @@ Size.in_bits size in
      let+ a = select_exp lang a in
      let w = const (Word.neg w) in
      ternop ldr word_ty mem a w
    | Load (mem, loc, _, size) ->
      let* mem = select_exp lang mem in
      let* loc = select_exp lang loc in
      ldr (Size.in_bits size) mem loc
    | Store (mem, BinOp (PLUS, Var a, Int w), value, _ , _size) ->
      let* mem = select_mem lang mem in
      let* value = select_exp lang value in
      str_base_off mem value a w
    | Store (mem, BinOp (MINUS, Var a, Int w), value, _ , _size) ->
      let* mem = select_mem lang mem in
      let* value = select_exp lang value in
      str_base_off mem value a Word.(-w)
    | Store (mem, loc, value, _, _size) ->
      let* mem = select_exp lang mem in
      let* loc = select_exp lang loc in
      let* value = select_exp lang value in
      (* We have to swap the arguments here, since ARM likes the value
         first, and the location second *)
      str mem value loc
    | BinOp (PLUS, a, Int w) when Word.(w = zero 32) ->
      select_exp lang a
    | BinOp (MINUS, a, Int w) when Word.(w = zero 32) ->
      select_exp lang a
    (* FIXME: this is amost certainly wrong *)
    | BinOp (PLUS, a, b) when Exp.(a = b) ->
      let* a = select_exp lang a in
      let zero = const (Word.zero 32) in
      let one = const (Word.one 32) in
      shl zero a one
    (* Thumb 2 encoding allows adding an 8-bit immediate, when
       source and destination registers are the same. *)
    | BinOp (PLUS, Var a, Int w)
    | BinOp (PLUS, Int w, Var a)
      when Core_kernel.(
          Option.exists lhs ~f:(Linear_ssa.same a)
          && thumb
          && Word.to_int_exn w <= 0xFF) ->
      KB.return @@ binop Ops.add word_ty (var a) (const w)
    (* Hack for loading a large constant. *)
    | BinOp (OR, Var a, BinOp (LSHIFT, Int w, Int s))
    | BinOp (OR, BinOp (LSHIFT, Int w, Int s), Var a)
      when Core_kernel.(
          Option.exists lhs ~f:(Linear_ssa.same a)
          && Word.(s = of_int ~width:32 16)) ->
      let op = Ir.simple_op Ops.movt
          (Ir.Var (create_temp word_ty))
          [Ir.Var (Ir.simple_var a); Ir.Const w] in
      KB.return {
        op_val = Ir.Var (create_temp word_ty);
        op_eff = instr op empty_eff}
    | BinOp (o, a, b) ->
      let* a = select_exp lang a in
      let* b = select_exp lang b in
      let* o = sel_binop o in
      o a b
    | UnOp (o, a) ->
      let+ a = select_exp lang a in
      sel_unop o a
    | Var v ->
      begin
        match Var.typ v with
        | Imm _ -> KB.return @@ var v
        | Mem _ -> KB.return @@ mem v
        | Unk -> Err.(fail @@ Other (
            Format.asprintf "select_exp: encountered variable %a of \
                             unknown type" Var.pp v))
      end
    | Int w -> KB.return @@ const w
    | Cast (_, _, _) -> Err.(fail @@ Other "select_exp: Cast is unsupported!")
    | Let (_, _, _) -> Err.(fail @@ Other "select_exp: Let is unsupported!")
    | Unknown (_, _) -> Err.(fail @@ Other "select_exp: Unknown is unsupported!")
    | Ite (_, _, _) -> Err.(fail @@ Other "select_exp: Ite is unsupported!")
    | Extract (_, _, _) -> Err.(fail @@ Other "select_exp: Extract is unsupported!")
    | Concat (_, _) -> Err.(fail @@ Other "select_exp: Concat is unsupported!")

  and select_mem (lang : Theory.language) (m : Bil.exp) : arm_pure KB.t =
    select_exp lang m

  and select_stmt (lang : Theory.language) (arg_vars : var list) (s : Blk.elt) : arm_eff KB.t =
    match s with
    | `Def t ->
      let lhs = Def.lhs t in
      let rhs = Def.rhs t in
      begin
        match Var.typ lhs with
        | Imm _ | Unk ->
          let* rhs = select_exp lang rhs ~lhs:(Some lhs) in
          let lhs = Ir.Var (Ir.simple_var lhs) in
          lhs := rhs
        | Mem _ ->
          let lhs_mem = Ir.Void (Ir.simple_var lhs) in
          let* rhs = select_exp lang rhs in
          lhs_mem := rhs
      end
    | `Jmp jmp ->
      let open KB.Syntax in
      let cond = Jmp.cond jmp in
      let is_call = is_call jmp in
      get_dst jmp >>= begin function
        | None -> Err.(fail @@ Other (
            Format.asprintf "Unexpected branch: %a" Jmp.pp jmp))
        (* NOTE: branches if cond is zero *)
        | Some dst -> match cond with
          | BinOp(EQ, cond, Int w) when Word.(w = zero 32) ->
            let+ cond = select_exp lang cond in
            br ~is_call:is_call cond dst lang
          | BinOp(NEQ, cond, Int w) when Word.(w = zero 32) ->
            let+ cond = select_exp lang cond in
            br ~is_call:is_call cond dst lang ~neg:true
          | Int w when Word.(w <> zero 32) ->
            KB.return @@ goto ~is_call:is_call dst arg_vars
          | cond ->
            (* XXX: this is a hack *)
            let neg = match cond with
              | BinOp (NEQ, _, _) -> true
              | _ -> false in
            let+ cond = select_exp lang cond in
            br ~is_call:is_call cond dst lang ~neg
      end
    | `Phi _ ->
      Err.(fail @@ Other
             "Arm_selector.select_stmt: Phi nodes are unsupported!")

  and select_elts (lang : Theory.language)
      (arg_vars : var list) (elts : Blk.elt list) : arm_eff KB.t =
    match elts with
    | [] -> KB.return empty_eff
    (* We only select 1 instruction at a time for now *)
    | s :: ss ->
      let* s = select_stmt lang arg_vars s in
      let+ ss = select_elts lang arg_vars ss in
      ss @. s

  and select_blk (tgt : Theory.target) (lang : Theory.language)
      (b : blk term) ~(argument_tids : Tid.Set.t) : arm_eff KB.t =
    let arg_vars, ignored =
      Term.enum def_t b |> Seq.fold ~init:(([], Tid.Set.empty), false)
        ~f:(fun ((acc, ignored), seen_mem) def ->
            let lhs = Def.lhs def and tid = Term.tid def in
            if Tid.Set.mem argument_tids tid
            then (lhs :: acc, ignored), seen_mem
            else match Var.typ lhs, Def.rhs def with
              | Mem _, Var m when not seen_mem ->
                (m :: acc, Tid.Set.add ignored tid), true
              | _ -> (acc, ignored), seen_mem) |> fst in
    let+ b_eff = Blk.elts b |> Seq.to_list |> List.filter ~f:(function
        | `Def d -> not @@ Tid.Set.mem ignored @@ Term.tid d
        | _ -> true) |> select_elts lang arg_vars in
    let {current_data; current_ctrl; other_blks} = b_eff in
    let new_blk =
      Ir.simple_blk (Term.tid b)
        (* data instructions are emitted in reverse chronological
           order *)
        ~data:(List.rev current_data)
        ~ctrl:(List.rev current_ctrl)
    in
    let all_blks = Ir.add new_blk other_blks in
    {
      current_data = [];
      current_ctrl = [];
      other_blks = all_blks
    }

  and select_blks (tgt : Theory.target) (lang : Theory.language)
      (bs : blk term list) ~(argument_tids : Tid.Set.t) : arm_eff KB.t =
    match bs with
    | [] -> KB.return empty_eff
    | b :: bs ->
      let* b = select_blk tgt lang b ~argument_tids in
      let+ bs = select_blks tgt lang bs ~argument_tids in
      b @. bs

  let select (tgt : Theory.target) (lang : Theory.language)
      (bs : blk term list) ~(argument_tids : Tid.Set.t) : Ir.t KB.t =
    let* bs = select_blks tgt lang bs ~argument_tids in
    ir bs

end



module Pretty = struct

  let opcode_pretty i : (string, Kb_error.t) result =
    Result.return @@ Ir.Opcode.name i

  (* We use this function when generating ARM, since the assembler
     doesn't like leading digits, % or @ in labels. *)
  let tid_to_string (t : tid) : string =
    let name = Tid.name t in
    let name =
      String.strip ~drop:Char.(fun c -> c = '%' || c = '@') name
    in
    let fst = String.get name 0 in
    let name =
      if Char.is_digit fst then
        "blk" ^ name
      else name
    in
    name

  type bracket =
      Open | Close | Neither | Both

  let arm_operand_pretty ~is_loc:is_loc (o : Ir.operand)
      : (string, Kb_error.t) result =
    let pretty_aux =
      match o with
      | Var v ->
        let error =
          Kb_error.Missing_semantics
            "arm_operand_pretty: operand.pre_assign field is empty" in
        let res =
          Result.map
            (Result.of_option v.pre_assign ~error:error)
            ~f:(fun reg -> Var.to_string reg)
        in
        res
      | Const w ->
        (* A little calisthenics to get this to look nice *)
        Result.return @@ Format.asprintf "#%a" Word.pp_dec w
      | Label l -> Result.return @@ tid_to_string l
      | Void _ -> Result.fail @@ Kb_error.Other "Tried printing a Void operand!"
      | Offset c ->
        (* Special printing of offsets to jump back from patched locations *)
        Result.return @@
        Format.asprintf "(%s + %d - %s)"
          Constants.patch_start_label
          (Word.to_int_exn c)
          Constants.patch_location
    in
    Result.map pretty_aux
      ~f:(fun p ->
          match is_loc with
          | Open -> Format.asprintf "[%s" p
          | Close -> Format.asprintf "%s]" p
          | Neither -> p
          | Both -> Format.asprintf "[%s]" p
        )

  let rm_void_args (args : Ir.operand list) : Ir.operand list =
    List.filter args
      ~f:(function | Void _ -> false | _ -> true)

  (* FIXME: Absolute hack *)
  (* We mark where the bracket location start and end in the argument list. *)
  let mk_loc_list (op : string) (args : 'a list) : bracket list =
    let len = List.length args in
    let init_neither len = List.init len ~f:(fun _ -> Neither) in
    if String.(op = "ldr" || op = "ldrh" || op = "ldrb" || op = "str") then
      begin
        if len = 2 then
          [Neither; Both]
        else if len = 3 then
          [Neither; Open; Close]
        else begin
          failwith @@
          sprintf "mk_loc_list: expected to receive 2 or 3 arguments, \
                   got %d (op = %s)" len op
        end
      end
    else
      init_neither len

  let arm_operands_pretty (op : string) (lhs : Ir.operand list) (rhs : Ir.operand list)
    : (string, Kb_error.t) result =
    (* Don't print the head of the operand if it's void. *)
    let rhs = if String.(op = "bl") then [List.hd_exn rhs] else rhs in
    let rhs = if String.(op = "movt") then List.tl_exn rhs else rhs in
    let l =
      if String.(op = "str")
      then rm_void_args rhs
      else rm_void_args (lhs @ rhs) in
    let is_loc_list = mk_loc_list op l in
    let l = List.zip_exn is_loc_list l in
    let all_str =
      List.map l
        ~f:(fun (is_loc, o) -> arm_operand_pretty ~is_loc:is_loc o) |>
      Result.all
    in
    let all_str = Result.map ~f:(List.intersperse ~sep:", ") all_str in
    Result.map ~f:String.concat all_str

  let arm_op_pretty (t : Ir.operation) : (string, Kb_error.t) result =
    let op = List.hd_exn t.opcodes in
    Result.(opcode_pretty op >>= fun op ->
            arm_operands_pretty op
              t.lhs
              t.operands >>= (fun operands ->
                  return (Format.asprintf "%s %s" op operands)))

  let arm_blk_pretty (t : Ir.blk) : (string list, Kb_error.t) result =
    let all_ops = t.data @ t.ctrl in
    let opcodes = List.map ~f:arm_op_pretty all_ops |> Result.all in
    let lab = Format.asprintf "%s:" (tid_to_string t.id) in
    Result.map opcodes ~f:(fun opcodes -> lab::opcodes)

  let arm_ir_pretty (t : Ir.t) : (string list, Kb_error.t) result =
    List.map ~f:arm_blk_pretty t.blks |> Result.all |> Result.map ~f:List.concat

end

(* Returns [true] if an instruction has no effect on data or
   control, is an overaproximation, of course. *)
let is_nop (op : Ir.operation) : bool =
  let open Ir in
  let { opcodes; lhs; operands; _ } = op in
  let opcode = List.hd_exn opcodes in
  let open ARM_ops.Ops in
  if Ir.Opcode.(opcode = mov) then
    begin
      match lhs, operands with
      | [Var a1], [Var a2] ->
        begin
          match (a1.pre_assign, a2.pre_assign) with
          (* r := r *)
          | Some v1, Some v2 -> Var.(v1 = v2)
          | _ -> false
        end
      | _ -> false
    end
  else if Ir.Opcode.(opcode = add || opcode = sub) then
    begin
      match lhs, operands with
      | [Var a1], [Var a2; Const w] ->
        begin
          match (a1.pre_assign, a2.pre_assign) with
          (* r := r +/- 0 *)
          | Some v1, Some v2 -> Var.(v1 = v2) && Word.(w = zero 32)
          | _ -> false
        end
      | _ -> false
    end
  else
    false

(* Removes spurious data operations *)
let filter_nops (ops : Ir.operation list) : Ir.operation list =
  List.filter ops ~f:(fun o -> not (is_nop o))

let peephole (ir : Ir.t) : Ir.t =
  let filter_nops blk =
    let {Ir.data; Ir.ctrl; _} = blk in
    let data = filter_nops data in
    let ctrl = filter_nops ctrl in
    { blk with
      data = data;
      ctrl = ctrl;
    }
  in
  let ir = Ir.map_blks ir ~f:filter_nops in
  ir
