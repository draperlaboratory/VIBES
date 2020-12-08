open !Core_kernel
open Bap.Std
open Bap_core_theory

module IR = Vibes_ir

open KB.Let

type arm_eff = {current_blk : IR.operation list; other_blks : IR.t} [@@deriving compare, equal]

let empty_eff = {current_blk = []; other_blks = IR.empty}

(* FIXME: if this is a constant, I'm pretty sure the op_eff field is always empty *)
type arm_pure = {op_val : IR.operand; op_eff : arm_eff} [@@deriving compare, equal]

(* We use this domain both for ['a pure] and ['s bitv] *)
let arm_pure_dom = KB.Domain.optional ~equal:equal_arm_pure "arm-pure"

let arm_pure =
  KB.Class.property Theory.Value.cls "arm-pure" arm_pure_dom

let reify_var (v : 'a Theory.var) : Bil.var = Var.reify v

let arm_eff_domain = KB.Domain.optional ~equal:equal_arm_eff "arm-eff"

let arm_eff = KB.Class.property Theory.Effect.cls "arm-eff" arm_eff_domain

let effect v = KB.Value.get arm_eff v

(* Explicitly aliasing for clarity *)
type arm_mem = arm_eff
let arm_mem = KB.Class.property Theory.Value.cls "arm-mem" arm_eff_domain

let (let=) v f = KB.(>>=) v
    (fun v ->
       match KB.Value.get arm_eff v with
       | None -> assert false
       | Some v -> f v)

let (let-) v f =
  KB.(>>=) v
    (fun v ->
      match KB.Value.get arm_pure v with
      | None -> assert false
      | Some v -> f v)

let (let/) v f =
  KB.(>>=) v
    (fun v ->
      match KB.Value.get arm_mem v with
      | None -> assert false
      | Some v -> f v)

let eff d =
  KB.return @@
    KB.Value.put arm_eff
      (Theory.Effect.empty Theory.Effect.Sort.bot)
      (Some d)

let memory m =
  KB.return @@
    KB.Value.put arm_mem
      (Theory.Value.empty (Theory.Mem.define (Theory.Bitv.define 32) (Theory.Bitv.define 32)))
  (Some m)

let pure v =
  KB.return @@
    KB.Value.put arm_pure
      (* This means we only have 32 bit vectors as our values *)
      (* TODO: extend this to handle memory, floats *)
      (Theory.Value.empty (Theory.Bitv.define 32))
      (Some v)


module ARM_ops = struct

let instr i sem = {current_blk = i::sem.current_blk; other_blks = sem.other_blks}

(* Some cowboy type checking here, to check which kind of mov to use.
   FIXME: certainly doesn't work if variables are instantiated with
   immediates! Can be fixed by having seperate Variable constructors
   for registers and immediates. *)
let arm_mov arg1 arg2 =
  let {op_val = arg2_var; op_eff = arg2_sem} = arg2 in
  let mov =
    match arg1, arg2_var with
    | IR.Var _, IR.Var _ -> IR.simple_op `MOVr arg1 [arg2_var]
    | IR.Var _, IR.Const _ -> IR.simple_op `MOVi arg1 [arg2_var]
    | _ -> failwith "arm_mov: unexpected arguments!"
  in
  instr mov arg2_sem

let ( := ) x y = arm_mov x y

(* TODO: only works for constants! *)
let bx addr =
  let i = IR.simple_op `BX (IR.Const addr) [] in
  instr i empty_eff

let jmp arg =
  let {op_val = arg_const; op_eff = arg_sem} = arg in
  let jmp =
    match arg_const with
    | Const w -> IR.simple_op `BX arg_const []
    | _ -> failwith "jmp: unexpected operand"
  in
  instr jmp arg_sem

let (@) s1 s2 =
  let {current_blk = blk1; other_blks = blks1} = s1 in
  let {current_blk = blk2; other_blks = blks2} = s1 in
  {current_blk = blk1 @ blk2; other_blks = IR.union blks1 blks2}

let var v = {op_val = IR.Var (IR.simple_var v); op_eff = empty_eff}

let const c = {op_val = IR.Const c; op_eff = empty_eff}

let uop o ty arg =
  let v = Var.create ~is_virtual:true ~fresh:true "temp" ty |> IR.simple_var in
  let {op_val = arg_val; op_eff = arg_sem} = arg in
  let op = IR.simple_op o (IR.Var v) [arg_val] in
  let sem = {arg_sem with current_blk = op::arg_sem.current_blk} in
  {op_val = IR.Var v; op_eff = sem}

let binop o ty arg1 arg2 =
  let v = Var.create ~is_virtual:true ~fresh:true "temp" ty |> IR.simple_var in
  let {op_val = arg1_val; op_eff = arg1_sem} = arg1 in
  let {op_val = arg2_val; op_eff = arg2_sem} = arg2 in
  let op = IR.simple_op o (IR.Var v) [arg1_val; arg2_val] in
  (* We do instruction ordering, of sorts, here. *)
  let sem = arg1_sem @ arg2_sem in
  let sem = {sem with current_blk = op::sem.current_blk} in
  {op_val = IR.Var v; op_eff = sem}

let (+) arg1 arg2 = binop `ADDrsi (Imm 32) arg1 arg2


let shl arg1 arg2 = binop `LSL (Imm 32) arg1 arg2


let ldr mem loc =
  (* Update the semantics of loc with those of mem *)
  let loc = {loc with op_eff = loc.op_eff @ mem} in
  uop `LDRrs (Imm 32) loc

let str mem loc value =
  let {op_val = loc_val; op_eff = loc_sem} = loc in
  let {op_val = value_val; op_eff = value_sem} = value in
  let op = IR.simple_op `STRrs loc_val [value_val] in
  (* Again, a little cowboy instruction ordering *)
  let sem = loc_sem @ value_sem @ mem in
  {sem with current_blk = op::sem.current_blk}



end

open KB.Let_syntax

module ARM_Core : Theory.Core =
struct
  include Theory.Empty
  include ARM_ops

  let set v arg =
    let arg_v =
      let r_var = v |> Var.reify in
      IR.simple_var r_var
    in
    let- arg = arg in
    eff ((IR.Var arg_v) := arg)

  let seq s1 s2 =
    let= s1 = s1 in
    let= s2 = s2 in
    eff @@ s1 @ s2

  let blk lab data ctrl =
    let= data = data in
    let= ctrl = ctrl in
    let new_instrs = List.append data.current_blk ctrl.current_blk in
    let new_blk = IR.simple_blk lab new_instrs in
    let all_blocks = IR.add new_blk @@ IR.union data.other_blks ctrl.other_blks in
    eff {current_blk = []; other_blks = all_blocks}

  let var (v : 'a Theory.var) : 'a Theory.pure =
    let sort = Theory.Var.sort v in
    let v = reify_var v in
    KB.return @@ KB.Value.put arm_pure (Theory.Value.empty sort)
      (Some (var v))

  let unk sort = assert false

  let let_ v e b = assert false

  let int sort (w : Theory.word) : 's Theory.bitv =
    let w = Bitvec.to_int32 w in
    KB.return @@
    KB.Value.put
      arm_pure
      (Theory.Value.empty @@ Theory.Bitv.define 32)
      (Some (const (Word.of_int32 w)))

  let add a b =
    let- a = a in
    let- b = b in
    pure @@ a + b

  let goto (lab : tid) : Theory.ctrl Theory.eff =
    let* conc_addr = KB.collect Theory.Label.addr lab in
    let conc_addr = Option.value_exn conc_addr in
    let conc_word = Word.create conc_addr 32 in
    eff @@ bx conc_word

  let jmp addr =
    let- addr_bitv = addr in
    eff @@ jmp addr_bitv

  let branch cond t_branch f_branch = assert false

  let repeat cond body = assert false

  let load mem loc =
    let/ mem = mem in
    let- loc = loc in
    pure @@ ldr mem loc

  let store mem loc value =
    let/ mem = mem in
    let- loc = loc in
    let- value = value in
    memory @@ str mem loc value

  let perform _sort = eff {current_blk = []; other_blks = IR.empty}

end

module BilARM = Theory.Parser.Make(ARM_Core)

let bil_stmt : type a. (a, Bil.exp, unit, Bil.stmt) Theory.Parser.stmt_parser =
  fun (module S) ->
  fun s ->
  match s with
  | Move (v, e) -> S.set_reg (Var.to_string v) 32 e
  | _ -> failwith "bil_stmt: not implemented"

let bil_bitv : type a. (a, Bil.exp, unit) Theory.Parser.bitv_parser =
  fun (module S) ->
  function Int w -> S.int (Word.to_bitvec w) (Word.bitwidth w)
         | Var v ->
           let sort = v |> Var.sort
                      |> Theory.Bitv.refine
                      |> Option.value ~default:(failwith "no sort!")
           in
           (* ??? that can't be right. *)
           S.var (Var.to_string v) (Theory.Bitv.size sort)
         | _ -> failwith "bil_bitv: not implemented"

let bil_bool : type a. (a, Bil.exp, unit) Theory.Parser.bool_parser =
  fun (module S) ->
  fun s -> assert false

let bil_mem : type a. (a, Bil.exp) Theory.Parser.mem_parser =
  fun (module S) ->
  fun s -> assert false

let bil_float : type a. (a, Bil.exp, unit) Theory.Parser.float_parser =
  fun (module S) ->
  fun s -> assert false

let bil_rmode : type a. (a, unit) Theory.Parser.rmode_parser =
  fun (module S) ->
  fun s -> S.rne

let bil_to_arm : (Bil.exp,unit,Bil.stmt) Theory.Parser.t =
  {
    bitv = bil_bitv;
    bool = bil_bool;
    mem = bil_mem;
    stmt = bil_stmt;
    float = bil_float;
    rmode = bil_rmode
  }

(* not sure what the right behavior is here... should we assume that a block is always created? *)
let ir (t : arm_eff) : IR.t =
  assert (List.is_empty t.current_blk);
  t.other_blks

let insn_pretty i =
  match i with
  | `MOVr
  | `MOVi -> "mov"
  | `BX -> "bx"
  | `ADDrsi -> "add"
  | `LSL -> "lsl"
  | `LDRrs -> "ldr"
  | `STRrs -> "str"
  | _ -> failwith "insn_pretty: instruction not supported"

let arm_operand_pretty (o : IR.operand) : string =
  match o with
  | Var v -> Var.to_string v.id
  | Const w ->
    (* A little calisthenics to get this to look nice *)
    Format.asprintf "#%a" Word.pp_dec w

let arm_operands_pretty (l : IR.operand list) : string =
  String.concat ~sep:"," (List.map l ~f:arm_operand_pretty)

let arm_op_pretty (t : IR.operation) : string =
  Format.asprintf "%s %s, %s"
    (* We just handle exactly one instruction *)
    (t.insns |> List.hd_exn |> insn_pretty)
    (t.lhs |> arm_operand_pretty)
    (t.operands |> arm_operands_pretty)

(* TODO: print the tid *)
let rec arm_blk_pretty (t : IR.blk) : string list = List.map ~f:arm_op_pretty t.operations

let arm_ir_pretty (t : IR.t) : string list =
  List.concat_map ~f:arm_blk_pretty t.blks
