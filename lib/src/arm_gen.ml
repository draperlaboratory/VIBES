open !Core_kernel
open Bap.Std
open Bap_core_theory

module IR = Vibes_ir

open KB.Let

let arm_abst_op_dom = KB.Domain.optional ~equal:IR.equal_operand "arm-abst-op"

let arm_abst_op =
  KB.Class.property Theory.Value.cls "arm-abst-op" arm_abst_op_dom

(* Some cowboy type checking here, to check which kind of mov to use.
   FIXME: certainly doesn't work if variables are instantiated with
   immediates! Can be fixed by having seperate Variable constructors
   for registers and immediates. *)
let arm_mov arg1 arg2 =
  match arg1, arg2 with
  | IR.Var _, IR.Var _ -> IR.simple_op `MOVr arg1 [arg2]
  | IR.Var _, IR.Const _ -> IR.simple_op `MOVi arg1 [arg2]
  | _ -> failwith "arm_mov: unexpected arguments!"

let arm_goto addr = IR.simple_op `BX (IR.Const addr) []

let reify_op (op : 'a Theory.pure) : IR.operand option KB.t =
  KB.(op >>| Value.get arm_abst_op)

let reify_var (v : 'a Theory.var) : Bil.var = Var.reify v

type arm_sem = {current_blk : IR.operation list; other_blks : IR.t} [@@deriving compare, equal]

let (@) s1 s2 =
  let { current_blk = blk1; other_blks = blks1} = s1 in
  let { current_blk = blk2; other_blks = blks2} = s2 in
  {current_blk = blk1 @ blk2; other_blks = IR.union blks1 blks2}

let arm_sem_domain = KB.Domain.optional ~equal:equal_arm_sem "arm-sem"

let arm_sem = KB.Class.property Theory.Effect.cls "arm-insn" arm_sem_domain

let effect v = KB.Value.get arm_sem v

let (>>:=) v f = KB.(>>=) v
    (fun v ->
       match effect v with
       | None -> assert false
       | Some v -> f v)

let eff d =
  KB.return @@
  KB.Value.put arm_sem
    (Theory.Effect.empty Theory.Effect.Sort.bot)
    (Some d)

let instr i =
  eff {current_blk = [i]; other_blks = IR.empty}


module ARM_Core : Theory.Core =
struct
  include Theory.Empty

  let set v arg =
    let arg_v =
      let r_var = v |> Var.reify in
      IR.simple_var r_var
    in
    let* arg_arg = reify_op arg in
    let arg_arg = Option.value_exn arg_arg in
    instr @@ arm_mov (IR.Var arg_v) arg_arg

  let seq s1 s2 =
    s1 >>:= fun s1 ->
    s2 >>:= fun s2 ->
    eff (s1 @ s2)

  let blk lab data ctrl =
    data >>:= fun data ->
    ctrl >>:=fun ctrl ->
    let new_instrs = List.append data.current_blk ctrl.current_blk in
    let new_blk = IR.simple_blk lab new_instrs in
    let all_blocks = IR.add new_blk @@ IR.union data.other_blks ctrl.other_blks in
    eff {current_blk = []; other_blks = all_blocks}

  let var (v : 'a Theory.var) : 'a Theory.pure =
    let sort = Theory.Var.sort v in
    let v = reify_var v in
    KB.return @@ KB.Value.put arm_abst_op (Theory.Value.empty sort)
      (Some (IR.Var (IR.simple_var v)))

  let unk _sort = assert false

  let let_ _v _e _b = assert false

  let int _sort (w : Theory.word) : 's Theory.bitv =
    let w = Bitvec.to_int32 w in
    KB.return @@
    KB.Value.put
      arm_abst_op
      (Theory.Value.empty @@ Theory.Bitv.define 32)
      (Some (IR.Const (Word.of_int32 w)))

  let goto (lab : tid) : Theory.ctrl Theory.eff =
    let* conc_addr = KB.collect Theory.Label.addr lab in
    let conc_addr = Option.value_exn conc_addr in
    let conc_word = Word.create conc_addr 32 in
    instr @@ arm_goto conc_word

  let jmp addr =
    let* addr_bitv = reify_op addr in
    match addr_bitv with
    | Some (Const w) ->
      instr @@ arm_goto w
    | _ -> failwith "jmp: unexpected operand"

  let branch _cond _t_branch _f_branch = assert false

  let repeat _cond _body = assert false

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
  fun _ -> assert false

let bil_mem : type a. (a, Bil.exp) Theory.Parser.mem_parser =
  fun (module S) ->
  fun _ -> assert false

let bil_float : type a. (a, Bil.exp, unit) Theory.Parser.float_parser =
  fun (module S) ->
  fun _ -> assert false

let bil_rmode : type a. (a, unit) Theory.Parser.rmode_parser =
  fun (module S) ->
  fun _ -> S.rne

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
let ir (t : arm_sem) : IR.t =
  assert (List.is_empty t.current_blk);
  t.other_blks

let insn_pretty (i : ARM.insn) =
  match i with
  | `MOVr
  | `MOVi -> "mov"
  | `BX -> "bx"
  | _ -> failwith "insn_pretty: instruction not supported"

let arm_operand_pretty (o : IR.operand) : string =
  match o with
  | Var v -> Var.to_string v.id
  | Const w ->
    (* A little calisthenics to get this to look nice *)
    Format.asprintf "#%a" Word.pp_dec w

let arm_op_pretty (t : IR.operation) : string =
  Format.asprintf "%s %s, %s"
    (t.insns |> List.hd_exn |> insn_pretty)
    (t.lhs |> arm_operand_pretty)
    (* We just handle exactly one operand *)
    (t.operands |> List.hd_exn |> arm_operand_pretty)

(* TODO: print the tid *)
let arm_blk_pretty (t : IR.blk) : string list = List.map ~f:arm_op_pretty t.operations

let arm_ir_pretty (t : IR.t) : string list =
  List.concat_map ~f:arm_blk_pretty t.blks
