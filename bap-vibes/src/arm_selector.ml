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
  let exclude = Theory.Role.Register.[
      stack_pointer;
      frame_pointer;
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

(* Assuming the var is in linear SSA form, we undo this form,
   and then check if it's a register name. *)
let reg_name (v : var) : string option =
  let name = Var.name v in
  let name = Option.value ~default:name (Linear_ssa.orig_name name) in
  Substituter.unmark_reg_name name

let is_stack_pointer (v : var) : bool = match reg_name v with
  | Some "SP" -> true
  | _ -> false

let preassign_var (pre : var option) (v : var)
    ~(is_thumb : bool) : var option =
  let typ = Var.typ v in
  match reg_name v with
  | None -> pre
  | Some "FP" ->
    (* We assign R11 as the pre-assigned FP register on ARM, and R7
       for Thumb, keeping in line with the ABI (as far as i can
       tell).  *)
    if is_thumb
    then Some (Var.create "R7" typ)
    else Some (Var.create "R11" typ)
  | Some name -> Some (Var.create name typ)

let preassign
    (_tgt : Theory.target)
    (ir : Ir.t)
    ~(is_thumb : bool) : Ir.t =
  let pre = preassign_var ~is_thumb in
  Ir.map_op_vars ir ~f:(fun v -> {
        v with pre_assign = List.hd_exn v.temps |> pre v.pre_assign})

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

  (* Condition codes used in ARM. *)
  module Cond = struct

    type t = EQ | NE | LE | GT | LT | GE | HI | LO | HS | LS

    let to_string : t -> string = function
      | EQ -> "eq"
      | NE -> "ne"
      | LE -> "le"
      | GT -> "gt"
      | LT -> "lt"
      | GE -> "ge"
      | HI -> "hi"
      | LO -> "lo"
      | HS -> "hs"
      | LS -> "ls"

    let of_string : string -> t option = function
      | "eq" -> Some EQ
      | "ne" -> Some NE
      | "le" -> Some LE
      | "gt" -> Some GT
      | "lt" -> Some LT
      | "ge" -> Some GE
      | "hi" -> Some HI
      | "lo" -> Some LO
      | "hs" -> Some HS
      | "ls" -> Some LS
      | _ -> None

    let opposite : t -> t = function
      | EQ -> NE
      | NE -> EQ
      | LE -> GT
      | GT -> LE
      | LT -> GE
      | GE -> LT
      | HI -> LS
      | LO -> HS
      | HS -> LO
      | LS -> HI

  end

  module Ops = struct

    include Ir.Opcode

    let op ?(cnd : Cond.t option = None) s =
      let s = s ^ Option.value_map cnd ~default:"" ~f:Cond.to_string in
      create ~arch:"arm" s

    (* With Thumb, there are cases where we need to set the flags to get the
       narrow encoding of the instruction (and other cases where this is the
       opposite). *)

    let movcc cnd = op "mov" ~cnd
    let mov set_flags = op (if set_flags then "movs" else "mov")
    let movw = op "movw"
    let add set_flags = op (if set_flags then "adds" else "add")
    let addw = op "addw"
    let mul = op "mul"
    let sub set_flags = op (if set_flags then "subs" else "sub")
    let neg = op "neg"
    let mvn = op "mvn"
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
    let b ?(cnd = None) () = op "b" ~cnd
    let bl ?(cnd = None) () = op "bl" ~cnd

  end

  let create_temp ty =
    Var.create ~is_virtual:true ~fresh:true "tmp" ty |>
    Ir.simple_var

  (* Helper data structure for generating conditional branches. *)
  module Branch = struct

    (* `generate` accepts the condition and returns the corresponding
       branch instruction, along with the fake destination operand.

       `is_call` denotes whether this is a conditional call or not.
    *)
    type t = {
      generate : Cond.t -> Ir.operand -> Ir.operation * Ir.operand;
      is_call : bool;
    }

    let create (dst : Ir.operand) ~(is_call : bool) : t = {
      generate = (fun cnd flg ->
          let cnd = Some cnd in
          let opcode =
            if is_call then Ops.(bl () ~cnd) else Ops.(b () ~cnd) in
          let tmp = Ir.Void (create_temp bit_ty) in
          let op = Ir.simple_op opcode tmp [dst; flg] in
          op, tmp);
      is_call;
    }

  end

  (* defaults to data instructions, since they are way more common *)
  let instr i sem =
    {sem with current_data = i::sem.current_data}

  (* Create a control statement. *)
  let control j sem =
    {sem with current_ctrl = j::sem.current_ctrl}

  (* Check if the constant is greater than 255, in which case we want 
     movw rather than mov. If it's greater than 65535, then we will use
     the ldr pseudo instruction. The assembler will store the constant
     in a literal pool at the end of our patch, and it will access this
     constant with a PC-relative load. *)
  let mov_const (c : word) ~(is_thumb : bool) : Ir.opcode =
    match Word.to_int_exn c with
    | n when n <= 0xFF -> Ops.mov is_thumb
    | n when n <= 0xFFFF -> Ops.movw
    | _ -> Ops.ldr 

  let is_movcc (o : Ir.operation) : bool =
    let n = Ir.Opcode.name @@ List.hd_exn o.opcodes in
    String.is_prefix n ~prefix:"mov" &&
    let n = String.drop_prefix n 3 in
    Option.is_some @@ Cond.of_string n

  (* Some cowboy type checking here, to check which kind of mov to
     use. Currently doesn't work if variables are instantiated
     with spilled registers! Can be fixed by having seperate Variable
     constructors for spilled and non-spilled registers. *)
  let arm_mov (arg1 : Ir.operand) (arg2 : arm_pure)
      ~(is_thumb : bool) : arm_eff KB.t =
    let {op_val = arg2_var; op_eff = arg2_sem} = arg2 in
    match arg1, arg2_var with
    | Ir.Var _, Ir.Var _
      when not @@ List.is_empty arg2_sem.current_data -> begin
        (* FIXME: absolute hack! if we have vars here, we can assume
           that the last operation assigned to a temporary, and we can
           just replace that temporary with the known destination, and
           return that as the effect. *)
        match arg2_sem.current_data with
        | [] -> assert false (* excluded by the guard above *)
        | o :: _ when is_movcc o ->
          (* This optimization should ignore conditional instructions.
             If the register allocator picks an optimal solution, then
             the peephole optimizer can get rid of this extra `mov`. *)
          let mov = Ir.simple_op Ops.(mov is_thumb) arg1 [arg2_var] in
          KB.return @@ instr mov arg2_sem
        | op :: ops ->
          let op = {op with Ir.lhs = [arg1]} in
          KB.return {arg2_sem with current_data = op :: ops}
      end
    | Ir.Var _, Ir.Var _ ->
      let mov = Ir.simple_op Ops.(mov is_thumb) arg1 [arg2_var] in
      KB.return @@ instr mov arg2_sem
    | Ir.Var _, Ir.Const w ->
      let mov = Ir.simple_op (mov_const w ~is_thumb) arg1 [arg2_var] in
      KB.return @@ instr mov arg2_sem
    | Ir.Void _, Ir.Void _
      when not @@ List.is_empty arg2_sem.current_data -> begin
        (* Same hack as above, but with void operands. *)
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

  let var v = {op_val = Ir.Var (Ir.simple_var v); op_eff = empty_eff}
  let mem v = {op_val = Ir.Void (Ir.simple_var v); op_eff = empty_eff}

  let const c = {op_val = Ir.Const c; op_eff = empty_eff}

  let uop o ty arg ~is_thumb =
    let res = create_temp ty in
    let {op_val = arg_val; op_eff = arg_sem} = arg in
    match arg_val with
    | Ir.Const w ->
      let tmp = Ir.Var (create_temp word_ty) in
      let ldr = Ir.simple_op (mov_const w ~is_thumb) tmp [arg_val] in
      let op = Ir.simple_op o (Ir.Var res) [tmp] in
      let sem = {arg_sem with current_data = op::ldr::arg_sem.current_data} in
      {op_val = Ir.Var res; op_eff = sem}
    | _ ->
      let op = Ir.simple_op o (Ir.Var res) [arg_val] in
      let sem = {arg_sem with current_data = op::arg_sem.current_data} in
      {op_val = Ir.Var res; op_eff = sem}

  let binop o ty arg1 arg2 ~is_thumb =
    let res = create_temp ty in
    let {op_val = arg1_val; op_eff = arg1_sem} = arg1 in
    let {op_val = arg2_val; op_eff = arg2_sem} = arg2 in
    match arg2_val with
    | Ir.Const w when Word.to_int_exn w > 0xFFF ->
      (* For binops that allow constant operands, the limit seems
         to be 12 bits according to the manual. *)
      let tmp = Ir.Var (create_temp word_ty) in
      let ldr = Ir.simple_op (mov_const w ~is_thumb) tmp [arg2_val] in
      let op = Ir.simple_op o (Ir.Var res) [arg1_val; tmp] in
      let sem = arg1_sem @. arg2_sem in
      let sem = {sem with current_data = op::ldr::sem.current_data} in
      {op_val = Ir.Var res; op_eff = sem}
    | _ ->
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

  let add (arg1 : arm_pure) (arg2 : arm_pure)
      ~(is_thumb : bool) : arm_pure KB.t =
    let {op_val; _} = arg2 in
    let fits_in_3 w =
      let open Word in
      let width = bitwidth w in
      of_int ~width 0 <= w && w <= of_int ~width 7
    in
    match op_val with
    | Const w when not (fits_in_3 w) && Word.to_int_exn w <= 0xFFF ->
      (* addw can accept up to 12 bits for the immediate. *)
      KB.return @@ binop Ops.addw word_ty arg1 arg2 ~is_thumb
    | _ ->
      KB.return @@ binop Ops.(add is_thumb) word_ty arg1 arg2 ~is_thumb

  let neg (arg : arm_pure) ~(is_thumb : bool) : arm_pure KB.t =
    KB.return @@ uop Ops.neg word_ty arg ~is_thumb

  let lognot (arg : arm_pure) ~(is_thumb : bool) : arm_pure KB.t =
    KB.return @@ uop Ops.mvn word_ty arg ~is_thumb

  let mul (arg1 : arm_pure) (arg2 : arm_pure)
      ~(is_thumb : bool) : arm_pure KB.t =
    KB.return @@ binop Ops.mul word_ty arg1 arg2 ~is_thumb

  let sub (arg1 : arm_pure) (arg2 : arm_pure)
      ~(is_thumb : bool) : arm_pure KB.t =
    KB.return @@ binop Ops.(sub is_thumb) word_ty arg1 arg2 ~is_thumb

  let sdiv (arg1 : arm_pure) (arg2 : arm_pure)
      ~(is_thumb : bool) : arm_pure KB.t =
    KB.return @@ binop Ops.sdiv word_ty arg1 arg2 ~is_thumb

  let udiv (arg1 : arm_pure) (arg2 : arm_pure)
      ~(is_thumb : bool) : arm_pure KB.t =
    KB.return @@ binop Ops.udiv word_ty arg1 arg2 ~is_thumb

  let lsl_ (arg1 : arm_pure) (arg2 : arm_pure)
      ~(is_thumb : bool) : arm_pure KB.t =
    KB.return @@ binop Ops.lsl_ word_ty arg1 arg2 ~is_thumb

  let lsr_ (arg1 : arm_pure) (arg2 : arm_pure)
      ~(is_thumb : bool) : arm_pure KB.t =
    KB.return @@ binop Ops.lsr_ word_ty arg1 arg2 ~is_thumb

  let asr_ (arg1 : arm_pure) (arg2 : arm_pure)
      ~(is_thumb : bool) : arm_pure KB.t =
    KB.return @@ binop Ops.asr_ word_ty arg1 arg2 ~is_thumb

  let ldr_op (bits : int) : Ir.opcode KB.t =
    if bits = 32 then KB.return Ops.ldr
    else if bits = 16 then KB.return Ops.ldrh
    else if bits = 8 then KB.return Ops.ldrb
    else Err.(fail @@ Other (
        sprintf "Arm_selector.ldr: Loading a bit-width that is not \
                 8, 16 or 32 (got %d)!" bits))

  let ldr (bits : int) (mem : arm_pure) (loc : arm_pure)
      ~(is_thumb : bool) : arm_pure KB.t =
    let+ ldr = ldr_op bits in
    (* Update the semantics of loc with those of mem *)
    binop ldr word_ty mem loc ~is_thumb

  let str (mem : arm_pure) (value : arm_pure) (loc : arm_pure)
      ~(is_thumb : bool) : arm_pure KB.t =
    let res = create_temp mem_ty in
    let lhs = Ir.Void (create_temp mem_ty) in
    let {op_val = loc_val; op_eff = loc_sem} = loc in
    let {op_val = value_val; op_eff = value_sem} = value in
    let {op_val = mem_val; op_eff = mem_sem} = mem in
    let+ ops = match value_val with
      | Var _ ->
        let op = Ir.simple_op Ops.str lhs [mem_val; value_val; loc_val] in
        KB.return [op]
      | Const w ->
        let tmp = Ir.Var (create_temp word_ty) in
        let mov = Ir.simple_op (mov_const w ~is_thumb) tmp [value_val] in
        let op = Ir.simple_op Ops.str lhs [mem_val; tmp; loc_val] in
        KB.return [op; mov]
      | _ ->
        Err.(fail @@ Other (
            sprintf "str: unsupported `value` operand %s" @@
            Ir.pretty_operand value_val)) in
    let sem = loc_sem @. value_sem @. mem_sem in
    let sem = {sem with current_data = ops @ sem.current_data} in
    {op_val = Void res; op_eff = sem}

  (* Special case of the `str` instruction where the address that we are
     storing to is of the shape `base + off` (e.g. `str R0, [R1, #8]`). *)
  let str_base_off (mem : arm_pure) (value : arm_pure) (base : var) (off : word)
      ~(is_thumb : bool) : arm_pure KB.t =
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
      | Const w ->
        let tmp = Ir.Var (create_temp word_ty) in
        let mov = Ir.simple_op (mov_const w ~is_thumb) tmp [value_val] in
        let op = Ir.simple_op Ops.str lhs [mem_val; tmp; base; off] in
        KB.return [op; mov]
      | _ ->
        Err.(fail @@ Other (
            sprintf "str_base_off: unsupported `value` operand %s" @@
            Ir.pretty_operand value_val)) in
    let sem = value_sem @. mem_sem in
    let sem = {sem with current_data = ops @ sem.current_data} in
    {op_val = Void res; op_eff = sem}

  let logand (a : arm_pure) (b : arm_pure) ~(is_thumb : bool) : arm_pure KB.t =
    KB.return @@ binop Ops.and_ word_ty a b ~is_thumb

  let logor (a : arm_pure) (b : arm_pure) ~(is_thumb : bool) : arm_pure KB.t =
    KB.return @@ binop Ops.orr word_ty a b ~is_thumb

  let xor (a : arm_pure) (b : arm_pure) ~(is_thumb : bool) : arm_pure KB.t =
    KB.return @@ binop Ops.eor word_ty a b ~is_thumb

  (* Specialization of binops for generating comparisons. *)
  let binop_cmp
      (cond : Cond.t)
      (arg1 : arm_pure)
      (arg2 : arm_pure)
      ~(patch : Data.Patch.t option)
      ~(is_thumb : bool)
      ~(branch : Branch.t option) : arm_pure KB.t =
    let tmp_flag = Ir.Void (create_temp bit_ty) in
    let {op_val = arg1_val; op_eff = arg1_sem} = arg1 in
    let {op_val = arg2_val; op_eff = arg2_sem} = arg2 in
    let cmp = Ir.simple_op Ops.cmp tmp_flag [arg1_val; arg2_val] in
    let sem = arg1_sem @. arg2_sem in
    match branch with
    | Some {generate; is_call} ->
      (* The comparison is used by a branch instruction, so use the
         `generate` function to add it to the ctrl semantics. It should
         be the only one in the current block. *)
      let* () = match sem.current_ctrl with
        | [] -> KB.return ()
        | _ -> Err.fail @@ Other
            "Arm_selector.binop_cmp: encountered a branch with non-empty \
             ctrl semantics" in
      let current_data = cmp :: sem.current_data in
      let br, op_val = generate cond tmp_flag in
      let sem = {
        sem with current_data; current_ctrl = br :: sem.current_ctrl
      } in
      (* Keep in mind, `op_val` should be discarded. *)
      KB.return @@ {op_val; op_eff = sem}
    | None ->
      (* Store the result of the comparison in an intermediate destination. *)
      let tmp1 = create_temp word_ty in
      let tmp2 = create_temp word_ty in
      (* These temps need to be unique, but VIBES IR also needs to know
         that they are congruent (i.e. they must map to the same register). *)
      let* () = match patch with
        | None -> KB.return ()
        | Some patch ->
          let t1 = List.hd_exn tmp1.temps in
          let t2 = List.hd_exn tmp2.temps in
          let* () = Data.Patch.add_congruence patch (t1, t2) in
          Data.Patch.add_congruence patch (t2, t1) in
      (* On Thumb we generate `movs` for the compact encoding, but this would
         clobber the flags (which we want to avoid). The pattern we generate 
         will assume the condition is true first, and then clear the result
         if it is false. *)
      let then_ = Ops.mov false in
      let else_ = Ops.movcc @@ Some (Cond.opposite cond) in
      let then_ = Ir.simple_op then_ (Var tmp1) [Const Word.(one 32)] in
      let else_ = Ir.simple_op else_ (Var tmp2) [
          Const Word.(zero 32);
          tmp_flag;
          Var tmp1
        ] in
      let ops = [else_; then_; cmp] in
      let sem = {sem with current_data = ops @ sem.current_data} in
      KB.return @@ {op_val = Var tmp2; op_eff = sem}

  let equals
      ~(patch : Data.Patch.t option)
      ~(is_thumb : bool)
      ~(branch : Branch.t option) : arm_pure -> arm_pure -> arm_pure KB.t =
    binop_cmp EQ ~patch ~is_thumb ~branch

  let not_equals
      ~(patch : Data.Patch.t option)
      ~(is_thumb : bool)
      ~(branch : Branch.t option) : arm_pure -> arm_pure -> arm_pure KB.t =
    binop_cmp NE ~patch ~is_thumb ~branch

  let less_than
      (arg1 : arm_pure)
      (arg2 : arm_pure)
      ~(patch : Data.Patch.t option)
      ~(is_thumb : bool)
      ~(branch : Branch.t option) : arm_pure KB.t =
    match arg1.op_val with
    | Ir.Const _ -> binop_cmp HI arg2 arg1 ~patch ~is_thumb ~branch
    | _ -> binop_cmp LO arg1 arg2 ~patch ~is_thumb ~branch

  let less_or_equal
      (arg1 : arm_pure)
      (arg2 : arm_pure)
      ~(patch : Data.Patch.t option)
      ~(is_thumb : bool)
      ~(branch : Branch.t option) : arm_pure KB.t =
    match arg1.op_val with
    | Ir.Const _ -> binop_cmp HS arg2 arg1 ~patch ~is_thumb ~branch
    | _ -> binop_cmp LS arg1 arg2 ~patch ~is_thumb ~branch

  let signed_less_than
      (arg1 : arm_pure)
      (arg2 : arm_pure)
      ~(patch : Data.Patch.t option)
      ~(is_thumb : bool)
      ~(branch : Branch.t option) : arm_pure KB.t =
    match arg1.op_val with
    | Ir.Const _ -> binop_cmp GT arg2 arg1 ~patch ~is_thumb ~branch
    | _ -> binop_cmp LT arg1 arg2 ~patch ~is_thumb ~branch 

  let signed_less_or_equal
      (arg1 : arm_pure)
      (arg2 : arm_pure)
      ~(patch : Data.Patch.t option)
      ~(is_thumb : bool)
      ~(branch : Branch.t option) : arm_pure KB.t =
    match arg1.op_val with
    | Ir.Const _ -> binop_cmp GE arg2 arg1 ~patch ~is_thumb ~branch
    | _ -> binop_cmp LE arg1 arg2 ~patch ~is_thumb ~branch

  (* Unconditional jump *)
  let goto ?(is_call : bool = false) (tgt : Ir.operand)
      (call_params : Ir.operand list) : arm_eff =
    let opcode = if is_call then Ops.bl () else Ops.b () in
    (* XXX: should we figure out how to better describe the effects of the call?
       Ideally, we should say that the call will clobber the memory and all
       caller-save registers. We could probably do this at the BIR level, at
       the return successor of each call site, where we make each effect
       explicit. *)
    let tmp_branch = create_temp bit_ty in
    let params = [tgt] @ call_params in
    control (Ir.simple_op opcode (Void tmp_branch) params) empty_eff

end

(* We assume that a block is always created *)
let ir_of_arm_eff (t : arm_eff) : Ir.t KB.t =
  if not (List.is_empty t.current_data && List.is_empty t.current_ctrl)
  then Err.(fail @@ Other "Arm_selector.ir: expected empty data and ctrl")
  else KB.return t.other_blks


module ARM_Gen =
struct
  open ARM_ops

  let sel_binop
      (o : binop)
      ~(patch : Data.Patch.t option)
      ~(is_thumb : bool)
      ~(branch : Branch.t option) : (arm_pure -> arm_pure -> arm_pure KB.t) KB.t =
    match o with
    | PLUS -> KB.return @@ add ~is_thumb
    | MINUS -> KB.return @@ sub ~is_thumb
    | TIMES -> KB.return @@ mul ~is_thumb
    | DIVIDE -> KB.return @@ udiv ~is_thumb
    | SDIVIDE -> KB.return @@ sdiv ~is_thumb
    | LSHIFT -> KB.return @@ lsl_ ~is_thumb
    | RSHIFT -> KB.return @@ lsr_ ~is_thumb
    | ARSHIFT ->  KB.return @@ asr_ ~is_thumb
    | AND -> KB.return @@ logand ~is_thumb
    | OR -> KB.return @@ logor ~is_thumb
    | EQ -> KB.return @@ equals ~patch ~is_thumb ~branch
    | NEQ -> KB.return @@ not_equals ~patch ~is_thumb ~branch
    | LT -> KB.return @@ less_than ~patch ~is_thumb ~branch
    | LE -> KB.return @@ less_or_equal ~patch ~is_thumb ~branch
    | SLT -> KB.return @@ signed_less_than ~patch ~is_thumb ~branch
    | SLE -> KB.return @@ signed_less_or_equal ~patch ~is_thumb ~branch
    | XOR -> KB.return @@ xor ~is_thumb
    | MOD | SMOD -> Err.(fail @@ Other (
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

  let sel_unop (o : unop)
      ~(is_thumb : bool) : (arm_pure -> arm_pure KB.t) KB.t =
    match o with
    | NOT -> KB.return @@ lognot ~is_thumb
    | NEG -> KB.return @@ neg ~is_thumb

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
      (e : Bil.exp)
      ~(patch : Data.Patch.t option)
      ~(is_thumb : bool) : arm_pure KB.t =
    let exp = select_exp ~patch ~is_thumb ~branch:None ~lhs:None in
    let exp_binop_integer = select_exp_binop_integer ~patch ~is_thumb in
    match e with
    | Load (mem, BinOp (PLUS, a, Int w), _, size) ->
      let* mem = exp mem in
      let* ldr = ldr_op @@ Size.in_bits size in
      let+ a = exp a in
      let w = const w in
      ternop ldr word_ty mem a w
    | Load (mem, BinOp (MINUS, a, Int w), _, size) ->
      let* mem = exp mem in
      let* ldr = ldr_op @@ Size.in_bits size in
      let+ a = exp a in
      let w = const (Word.neg w) in
      ternop ldr word_ty mem a w
    | Load (mem, Int addr, _, size) ->
      let* mem = exp mem in
      let tmp = Ir.Var (create_temp word_ty) in
      let op = Ir.simple_op (mov_const addr ~is_thumb) tmp Ir.[Const addr] in
      let a = {op_val = tmp; op_eff = instr op empty_eff} in
      let+ ldr = ldr_op @@ Size.in_bits size in
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
    | Store (mem, Int addr, value, _, _size) ->
      let* mem = exp mem in
      let tmp = Ir.Var (create_temp word_ty) in
      let op = Ir.simple_op (mov_const addr ~is_thumb) tmp Ir.[Const addr] in
      let loc = {op_val = tmp; op_eff = instr op empty_eff} in
      let* value = exp value in
      str mem value loc ~is_thumb
    | Store (mem, loc, value, _, _size) ->
      let* mem = exp mem in
      let* loc = exp loc in
      let* value = exp value in
      (* We have to swap the arguments here, since ARM likes the value
         first, and the location second *)
      str mem value loc ~is_thumb
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
      when Option.exists lhs ~f:(Linear_ssa.same a)
        && is_thumb && Word.to_int_exn w <= 0xFF ->
      let set_flags = not @@ is_stack_pointer a in
      let op = if Caml.(o = PLUS) then Ops.add else Ops.sub in
      KB.return @@ binop (op set_flags) word_ty (var a) (const w) ~is_thumb
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
      let* o = sel_binop o ~patch ~is_thumb ~branch in
      o a b
    | UnOp (o, a) -> begin
        match o, Type.infer a with
        | _ , Error e ->
          Err.fail @@ Other
            (sprintf "Arm_selector.select_exp: Type.infer failed: %s"
               (Type.Error.to_string e))
        | NOT, Ok (Imm 1) ->
          (* Lazy way to compute the negation of that boolean. *)
          let identity = Bil.(BinOp (EQ, a, Int (Word.zero 32))) in
          select_exp identity ~patch ~branch ~is_thumb ~lhs:None
        | _ ->
          let* a = exp a in
          let* o = sel_unop o ~is_thumb in
          o a
      end
    | Var v -> begin
        match Var.typ v with
        | Imm 1 when Option.is_some branch ->
          (* Lazy way to compute the boolean. *)
          let v = var v in
          let c = const @@ Word.zero 32 in
          let* o = sel_binop NEQ ~patch ~is_thumb ~branch in
          o v c
        | _ when Option.is_some branch ->
          Err.(fail @@ Other
                 "select_exp: Ill-typed variable in the condition \
                  of a branch")
        | Imm _ -> KB.return @@ var v
        | Mem _ -> KB.return @@ mem v
        | Unk -> Err.(fail @@ Other (
            Format.asprintf "select_exp: encountered variable %a of \
                             unknown type" Var.pp v))
      end
    | Cast (UNSIGNED, _, e) -> select_exp e ~patch ~is_thumb ~branch ~lhs
    | Cast (SIGNED, _, _) ->
      Err.(fail @@ Other "select_exp: SIGNED Cast is unsupported!")
    | Cast (LOW, _, _) ->
      Err.(fail @@ Other "select_exp: LOW Cast is unsupported!")
    | Cast (HIGH, _, _) ->
      Err.(fail @@ Other "select_exp: HIGH Cast is unsupported!")
    | Int w -> KB.return @@ const w
    | Let (_, _, _) -> Err.(fail @@ Other "select_exp: Let is unsupported!")
    | Unknown (_, _) -> Err.(fail @@ Other "select_exp: Unknown is unsupported!")
    | Ite (_, _, _) -> Err.(fail @@ Other "select_exp: Ite is unsupported!")
    | Extract (_, _, _) -> Err.(fail @@ Other "select_exp: Extract is unsupported!")
    | Concat (_, _) -> Err.(fail @@ Other "select_exp: Concat is unsupported!")

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
      ~(patch : Data.Patch.t option)
      ~(is_thumb : bool) : arm_pure KB.t =
    let* rhs = select_exp rhs ~patch ~is_thumb in
    let* o = sel_binop o ~patch ~is_thumb ~branch:None in
    let tmp = Ir.Var (create_temp word_ty) in
    let op = Ir.simple_op Ops.(mov is_thumb) tmp [Const lhs] in
    let lhs = {op_val = tmp; op_eff = instr op empty_eff} in
    if swap then o rhs lhs else o lhs rhs

  and select_stmt
      (call_params : Ir.operand list)
      (s : Blk.elt)
      ~(patch : Data.Patch.t option)
      ~(is_thumb : bool) : arm_eff KB.t =
    let exp = select_exp ~patch ~is_thumb in
    match s with
    | `Def t -> begin
        let lhs = Def.lhs t in
        let rhs = Def.rhs t in
        let mov = arm_mov ~is_thumb in
        match Var.typ lhs with
        | Imm _ | Unk ->
          let* rhs = exp rhs ~lhs:(Some lhs) in
          let lhs = Ir.Var (Ir.simple_var lhs) in
          mov lhs rhs
        | Mem _ ->
          let lhs_mem = Ir.Void (Ir.simple_var lhs) in
          (* We don't need to pass the lhs for mem assign, since
             none of the patterns we match against will apply here. *)
          let* rhs = exp rhs in
          mov lhs_mem rhs
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
          | Int w when Word.(w <> b0) ->
            KB.return @@ goto dst call_params ~is_call
          | _ ->
            let+ {op_eff = eff; _} =
              exp cond ~branch:(Some (Branch.create dst ~is_call)) in
            eff
      end
    | `Phi _ -> KB.return empty_eff

  and select_elts
      (call_params : Ir.operand list)
      (elts : Blk.elt list)
      ~(patch : Data.Patch.t option)
      ~(is_thumb : bool) : arm_eff KB.t =
    match elts with
    | [] -> KB.return empty_eff
    (* We only select 1 instruction at a time for now *)
    | s :: ss ->
      let* s = select_stmt call_params s ~patch ~is_thumb in
      let+ ss = select_elts call_params ss ~patch ~is_thumb in
      ss @. s

  and select_blk
      (b : blk term)
      ~(patch : Data.Patch.t option)
      ~(is_thumb : bool)
      ~(argument_tids : Tid.Set.t) : arm_eff KB.t =
    (* NOTE: `argument_tids` contains the set of def tids where we
       assign parameter registers before a call. These are guaranteed to
       be present in the same block of their corresponding call (see the
       implementation in Core_c).

       The same applies to the spurious memory assignment before the call.
       This is a signpost for Minizinc, so that it knows that the call
       depends on the current state of the memory. This way, the scheduler
       won't reorder loads and stores in a way that breaks the generated
       code.
    *)
    let* call_params, ignored =
      Term.enum def_t b |> KB.Seq.fold ~init:([], Tid.Set.empty)
        ~f:(fun (acc, ignored) def ->
            let lhs = Def.lhs def in
            let tid = Term.tid def in
            if Tid.Set.mem argument_tids tid then
              match Var.typ lhs, Def.rhs def with
              | Imm _, _ | Unk, _ ->
                KB.return (Ir.Var (Ir.simple_var lhs) :: acc, ignored)
              | Mem _, Var m ->
                (* We do not want to actually generate code for this.
                   It is just a signpost for the selector to collect the most
                   recent version of the memory so we can pass it as a
                   dependency of the call. *)
                KB.return (
                  Ir.Void (Ir.simple_var m) :: acc,
                  Tid.Set.add ignored tid)
              | Mem _, _ -> Err.fail @@ Other
                  (sprintf "Arm_selector.select_blk: unexpected RHS for call \
                            param mem %s at tid %s"
                     (Var.to_string lhs) (Tid.to_string tid))
            else KB.return (acc, ignored)) in
    let+ b_eff = Blk.elts b |> Seq.to_list |> List.filter ~f:(function
        | `Def d -> not @@ Tid.Set.mem ignored @@ Term.tid d
        | _ -> true) |> select_elts call_params ~patch ~is_thumb in
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

  and select_blks
      (bs : blk term list)
      ~(patch : Data.Patch.t option)
      ~(is_thumb : bool)
      ~(argument_tids : Tid.Set.t) : arm_eff KB.t =
    match bs with
    | [] -> KB.return empty_eff
    | b :: bs ->
      let* b = select_blk b ~patch ~is_thumb ~argument_tids in
      let+ bs = select_blks bs ~patch ~is_thumb ~argument_tids in
      b @. bs

  let select
      ?(patch : Data.Patch.t option = None)
      (bs : blk term list)
      ~(is_thumb : bool)
      ~(argument_tids : Tid.Set.t) : Ir.t KB.t =
    let* bs = select_blks bs ~patch ~is_thumb ~argument_tids in
    ir_of_arm_eff bs

end

module Isel = struct
  open Isel.Utils
  let patterns : (Isel.Pattern.t * Isel.Template.t) String.Map.t =
    String.Map.of_alist_exn [
      "add", binop PLUS (ARM_ops.Ops.add false);
      "mov", mov (ARM_ops.Ops.mov false);
      "str", store (Ir.Opcode.create "str");
      "ld",  load (Ir.Opcode.create "ld");
      "b",   goto (Ir.Opcode.create "b");
      "null_jump", null_jump (Ir.Opcode.create "b")
    ]

end

module Pretty = struct

  type opc = (string, string * string) Either.t

  let opcode_pretty
      (i : Ir.opcode)
      ~(is_thumb : bool) : (opc, Kb_error.t) result =
    let module C = ARM_ops.Cond in
    let name = Ir.Opcode.name i in
    let it n =
      if is_thumb then match C.of_string @@ String.drop_prefix name n with
        | None -> Result.return @@ First name
        | Some cc -> Result.return @@ Second ("it " ^ C.to_string cc, name)
      else Result.return @@ First name in
    if String.is_prefix name ~prefix:"bl" then it 2
    else if String.is_prefix name ~prefix:"mov" then it 3
    else Result.return @@ First name

  (* We use this function when generating ARM, since the assembler
     doesn't like leading digits, % or @ in labels. *)
  let tid_to_string (t : tid) : string =
    let name =
      Tid.name t |> String.strip ~drop:Char.(fun c -> c = '%' || c = '@') in
    if Char.is_digit name.[0] then "blk" ^ name else name

  type bracket = Open | Close | Neither | Both

  let arm_operand_pretty (op : string) (o : Ir.operand)
      ~(is_loc : bracket) : (string, Kb_error.t) result =
    let pretty_aux =
      match o with
      | Var v ->
        let error =
          Kb_error.Missing_semantics
            "arm_operand_pretty: operand.pre_assign field is empty" in
        Result.of_option v.pre_assign ~error |> Result.map ~f:Var.to_string
      | Const w ->
        (* A little calisthenics to get this to look nice *)
        let prefix = match op with
          | "ldr" -> begin
              match is_loc with
              | Neither -> '='
              | _ -> '#'
            end
          | _ -> '#' in
        Result.return @@ Format.asprintf "%c%a" prefix Word.pp_dec w
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

  let rm_void_args : Ir.operand list -> Ir.operand list =
    List.filter ~f:(function Ir.Void _ -> false | _ -> true)

  let is_const : Ir.operand -> bool = function
    | Const _ -> true
    | _ -> false

  (* FIXME: Absolute hack *)
  (* We mark where the bracket location start and end in the argument list. *)
  let mk_loc_list
      (op : string)
      (args : Ir.operand list) : (bracket list, Kb_error.t) result =
    let len = List.length args in
    let init_neither len = List.init len ~f:(fun _ -> Neither) in
    match op with
    | "ldr" when len = 2 && is_const (List.nth_exn args 1) ->
      Result.return [Neither; Neither]
    | "ldr" | "ldrh" | "ldrb" | "str" ->
      if len = 2 then Result.return [Neither; Both]
      else if len = 3 then Result.return [Neither; Open; Close]
      else Result.fail @@ Kb_error.Other (
          sprintf "mk_loc_list: expected to receive 2 or 3 arguments, \
                   got %d (op = %s)" len op)
    | _ -> Result.return @@ init_neither len

  let arm_operands_pretty
      (op : string)
      (lhs : Ir.operand list)
      (rhs : Ir.operand list) : (string, Kb_error.t) result =
    let open Result.Monad_infix in
    (* bl may have pseudo-arguments, so ignore them. *)
    let rhs = if String.(op = "bl") then [List.hd_exn rhs] else rhs in
    (* conditional move may have pseudo-arguments at the end *)
    let rhs =
      if String.is_prefix op ~prefix:"mov" &&
         String.length op > 3
      then [List.hd_exn rhs]
      else rhs in
    let l = rm_void_args (lhs @ rhs) in
    mk_loc_list op l >>= fun is_loc_list ->
    List.zip_exn is_loc_list l |>
    List.map ~f:(fun (is_loc, o) -> arm_operand_pretty op o ~is_loc) |>
    Result.all >>| fun all_str ->
    String.concat @@ List.intersperse all_str ~sep:", "

  let arm_op_pretty
      (t : Ir.operation)
      ~(is_thumb : bool) : (string list, Kb_error.t) result =
    let open Result.Monad_infix in
    List.hd_exn t.opcodes |> opcode_pretty ~is_thumb >>= fun op ->
    let o = match op with
      | First o -> o
      | Second (_, o) -> o in
    arm_operands_pretty o t.lhs t.operands >>| fun ops -> match op with
    | First op -> [Format.asprintf "%s %s" op ops]
    | Second (o1, o2) -> [o1; Format.asprintf "%s %s" o2 ops]

  let arm_blk_pretty
      (t : Ir.blk)
      ~(is_thumb : bool) : (string list, Kb_error.t) result =
    let open Result.Monad_infix in
    let all_ops = t.data @ t.ctrl in
    List.map ~f:(arm_op_pretty ~is_thumb) all_ops |>
    Result.all >>| fun opcodes ->
    Format.asprintf "%s:" (tid_to_string t.id) ::
    List.concat opcodes

  let arm_ir_pretty
      (t : Ir.t)
      ~(is_thumb : bool) : (string list, Kb_error.t) result =
    List.map ~f:(arm_blk_pretty ~is_thumb) t.blks |>
    Result.all |> Result.map ~f:List.concat

end

(* Returns [true] if an instruction has no effect on data or
   control, is an overaproximation, of course. *)
let is_nop (op : Ir.operation) : bool =
  let open Ir in
  let { opcodes; lhs; operands; _ } = op in
  let opcode = List.hd_exn opcodes in
  match Ir.Opcode.name opcode with
  | "mov" | "movs" -> begin
      match lhs, operands with
      | [Var a1], [Var a2] -> begin
          match (a1.pre_assign, a2.pre_assign) with
          (* r := r *)
          | Some v1, Some v2 -> Var.(v1 = v2)
          | _ -> false
        end
      | _ -> false
    end
  | "add" | "adds" | "sub" | "subs" -> begin
      match lhs, operands with
      | [Var a1], [Var a2; Const w] -> begin
          match (a1.pre_assign, a2.pre_assign) with
          (* r := r +/- 0 *)
          | Some v1, Some v2 -> Var.(v1 = v2) && Word.(w = zero 32)
          | _ -> false
        end
      | _ -> false
    end
  | _ -> false

(* Removes spurious data operations *)
let filter_nops (ops : Ir.operation list) : Ir.operation list =
  List.filter ops ~f:(fun o -> not (is_nop o))

let implicit_fallthroughs (ir : Ir.t) : Ir.t =
  let rec interleave_pairs = function
    | x :: y :: rest -> (x, y) :: interleave_pairs (y :: rest)
    | [] | [_] -> [] in
  let afters =
    interleave_pairs ir.blks |>
    List.fold ~init:Tid.Map.empty ~f:(fun afters (x, y) ->
        Map.set afters ~key:x.id ~data:y.id) in
  Ir.map_blks ir ~f:(fun blk ->
      (* Find the last control operation. *)
      match List.last blk.ctrl with
      | Some o -> begin
          (* Is it an unconditional branch? *)
          let op = List.hd_exn o.opcodes in
          match Ir.Opcode.name op with
          | "b" -> begin
              (* Is the target of the branch the immediate next block in
                 the ordering? *)
              match List.hd_exn o.operands, Map.find afters blk.id with
              | Label id, Some id' when Tid.(id = id') ->
                (* Delete the branch in favor of an implicit fallthrough. *)
                {blk with ctrl = List.drop_last_exn blk.ctrl}
              | _ -> blk
            end
          | _ -> blk
        end
      | None -> blk)

let peephole (ir : Ir.t) (_cfg : Graphs.Tid.t) : Ir.t =
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
  let ir = implicit_fallthroughs ir in
  ir
