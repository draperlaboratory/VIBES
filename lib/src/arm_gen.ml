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

let (@) s1 s2 =
  let { current_blk = blk1; other_blks = blks1} = s1 in
  let { current_blk = blk2; other_blks = blks2} = s2 in
  {current_blk = blk1 @ blk2; other_blks = IR.union blks1 blks2}

let arm_eff_domain = KB.Domain.optional ~equal:equal_arm_eff "arm-eff"

let arm_eff = KB.Class.property Theory.Effect.cls "arm-eff" arm_eff_domain

let effect v = KB.Value.get arm_eff v

let arm_mem = KB.Class.property Theory.Value.cls "arm-mem" arm_eff_domain

let (let=) v f = KB.(>>=) v
    (fun v ->
       match KB.Value.get arm_eff v with
       | None -> Errors.fail Errors.Missing_property
       | Some v -> f v)

let (let-) v f =
  KB.(>>=) v
    (fun v ->
      match KB.Value.get arm_pure v with
      | None -> Errors.fail Errors.Missing_property
      | Some v -> f v)

let (let/) v f =
  KB.(>>=) v
    (fun v ->
      match KB.Value.get arm_mem v with
      | None -> Errors.fail Errors.Missing_property
      | Some v -> f v)

let eff d =
  KB.return @@
    KB.Value.put arm_eff
      (Theory.Effect.empty Theory.Effect.Sort.bot)
      (Some d)

(* type r32 *)

type 'a sort = 'a Theory.Bitv.t Theory.Value.sort

(* We make this polymorphic, so that it can be instantiated in any
   setting, despite being a fixed given size. We add a [unit] argument
   to avoid the value restriction. *)
(* TODO: fix this, so that s32 is of type r32 sort *)
let s32 (_ : unit) : 'a sort = Theory.Bitv.define 32

let memory m =
  KB.return @@
    KB.Value.put arm_mem
      (Theory.Value.empty (Theory.Mem.define (s32 ()) (s32 ())))
  (Some m)

let pure v =
  KB.return @@
    KB.Value.put arm_pure
      (* This means we only have 32 bit vectors as our values *)
      (* TODO: extend this arbitrary sizes *)
      (Theory.Value.empty (s32 ()))
      (Some v)

let bool b =
  KB.return @@
    KB.Value.put arm_pure
      (* This means we only have 32 bit vectors as our values *)
      (* TODO: extend this arbitrary sizes *)
      (Theory.Value.empty Theory.Bool.t)
      (Some b)

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
    | Const _ -> IR.simple_op `BX arg_const []
    | _ -> failwith "jmp: unexpected operand"
  in
  instr jmp arg_sem

let var v = {op_val = IR.Var (IR.simple_var v); op_eff = empty_eff}

let const c = {op_val = IR.Const c; op_eff = empty_eff}

let uop o ty arg =
  let res = Var.create ~is_virtual:true ~fresh:true "temp" ty |> IR.simple_var in
  let {op_val = arg_val; op_eff = arg_sem} = arg in
  let op = IR.simple_op o (IR.Var res) [arg_val] in
  let sem = {arg_sem with current_blk = op::arg_sem.current_blk} in
  {op_val = IR.Var res; op_eff = sem}

let binop o ty arg1 arg2 =
  let res = Var.create ~is_virtual:true ~fresh:true "temp" ty |> IR.simple_var in
  let {op_val = arg1_val; op_eff = arg1_sem} = arg1 in
  let {op_val = arg2_val; op_eff = arg2_sem} = arg2 in
  let op = IR.simple_op o (IR.Var res) [arg1_val; arg2_val] in
  (* We do instruction ordering, of sorts, here. *)
  let sem = arg1_sem @ arg2_sem in
  let sem = {sem with current_blk = op::sem.current_blk} in
  {op_val = IR.Var res; op_eff = sem}

(* TODO: handle non-register arguments? *)
let (+) arg1 arg2 = binop `ADDrsi (Imm 32) arg1 arg2

let shl _signed arg1 arg2 = binop `LSL (Imm 32) arg1 arg2

let shr signed arg1 arg2 =
  let b =
    match signed.op_val with
    | IR.Const w -> (Word.to_int_exn w) <> 0
    (* Not sure what to do here *)
    | _ -> assert false
  in
  if b then
    binop `ASR (Imm 32) arg1 arg2
  else
    binop `LSR (Imm 32) arg1 arg2

let ldr mem loc =
  (* Update the semantics of loc with those of mem *)
  let loc = {loc with op_eff = loc.op_eff @ mem} in
  uop `LDRrs (Imm 32) loc

let str mem value loc =
  let {op_val = loc_val; op_eff = loc_sem} = loc in
  let {op_val = value_val; op_eff = value_sem} = value in
  let op = IR.simple_op `STRrs loc_val [value_val] in
  (* Again, a little cowboy instruction ordering *)
  let sem = loc_sem @ value_sem @ mem in
  {sem with current_blk = op::sem.current_blk}

let (&&) a b = binop `ANDrsi (Imm 32) a b

let (||) a b = binop `ORRrsi (Imm 32) a b

let xor a b = binop `EORrsi (Imm 32) a b

end


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

  let unk _sort = Errors.fail (Errors.Not_implemented "Arm_gen.unk")

  let let_ _v _e _b = Errors.fail (Errors.Not_implemented "Arm_gen.let_")

  let int sort (w : Theory.word) : 's Theory.bitv =
  (* This is incorrect: we're assuming every constant is exactly 32
     bits. *)
    let w = Bitvec.to_int32 w in
    KB.return @@
    KB.Value.put
      arm_pure
      (Theory.Value.empty sort)
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

  let branch _cond _t_branch _f_branch = Errors.fail (Errors.Not_implemented "Arm_gen.branch")

  let repeat _cond _body = Errors.fail (Errors.Not_implemented "Arm_gen.repeat")

  let load mem loc =
    let/ mem = mem in
    let- loc = loc in
    pure @@ ldr mem loc

  let store mem loc value =
    let/ mem = mem in
    let- loc = loc in
    let- value = value in
    (* We have to swap the arguments here, since ARM likes the value
       first, and the location second *)
    memory @@ str mem value loc

  let perform _sort = eff {current_blk = []; other_blks = IR.empty}

  let shiftl sign l r =
    let- sign = sign in
    let- l = l in
    let- r = r in
    pure @@ shl sign l r

  let shiftr sign l r =
    let- sign = sign in
    let- l = l in
    let- r = r in
    pure @@ shr sign l r

  let and_ a b =
    let- a = a in
    let- b = b in
    bool (a && b)

  let or_ a b =
    let- a = a in
    let- b = b in
    bool (a || b)

  let logand a b =
    let- a = a in
    let- b = b in
    pure (a && b)

  let logor a b =
    let- a = a in
    let- b = b in
    pure (a || b)

  let logxor a b =
    let- a = a in
    let- b = b in
    pure @@ xor a b

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
let ir (t : arm_eff) : IR.t =
  assert (List.is_empty t.current_blk);
  t.other_blks

let insn_pretty i : (string, Errors.t) result =
  match i with
  | `MOVr
  | `MOVi   -> Ok "mov"
  | `BX     -> Ok "bx"
  | `ADDrsi -> Ok "add"
  | `LSL    -> Ok "lsl"
  | `LSR    -> Ok "lsr"
  | `ASR    -> Ok "asr"
  | `ANDrsi -> Ok "and"
  | `ORRrsi -> Ok "orr"
  | `EORrsi -> Ok "eor"
  | `LDRrs  -> Ok "ldr"
  | `STRrs  -> Ok "str"
  | i       ->
     let to_string _ = "UNKNOWN" in
     let msg = Format.asprintf "insn_pretty: instruction %s not supported"
                 (to_string i)
     in
     Error (Errors.Not_implemented msg)

type op_tag = Mem | Not_mem

(* TODO: refactor this into insn_pretty? *)
let tags_of_op i : (op_tag list, Errors.t) result =
  match i with
  | `MOVr   -> Ok [Not_mem]
  | `MOVi   -> Ok [Not_mem]
  | `BX     -> Ok [Not_mem]
  | `ADDrsi -> Ok [Not_mem; Not_mem]
  | `LSL    -> Ok [Not_mem; Not_mem]
  | `LDRrs  -> Ok [Mem]
  | `STRrs  -> Ok [Mem]
  | `LSR    -> Ok [Not_mem; Not_mem]
  | `ASR    -> Ok [Not_mem; Not_mem]
  | `ANDrsi -> Ok [Not_mem; Not_mem]
  | `ORRrsi -> Ok [Not_mem; Not_mem]
  | `EORrsi -> Ok [Not_mem; Not_mem]
  | i       ->
     let to_string _ = "UNKNOWN" in
     let msg = Format.asprintf "tags_of_op: instruction %s not supported"
                 (to_string i)
     in
     Error (Errors.Not_implemented msg)


let arm_operand_pretty ?tag:(tag = Not_mem) (o : IR.operand) : string =
  match o with
  | Var v ->
     let v = Var.to_string v.id in
     begin
       match tag with
       | Mem ->
          Format.asprintf "[%s]" v
       | Not_mem ->
          Format.asprintf "%s" v
     end
  | Const w ->
    (* A little calisthenics to get this to look nice *)
    Format.asprintf "#%a" Word.pp_dec w

let arm_operands_pretty (tags : op_tag list) (l : IR.operand list) : string =
  String.concat ~sep:", "
    (List.map2_exn tags l ~f:(fun t o -> arm_operand_pretty ~tag:t o))

let arm_op_pretty (t : IR.operation) : (string, Errors.t) result =
  let op = List.hd_exn t.insns in
  let op_tags = tags_of_op op in
  Result.(insn_pretty op >>= fun op ->
          op_tags >>| fun op_tags ->
          Format.asprintf "%s %s, %s"
            (* We just handle exactly one instruction *)
            op
            (arm_operand_pretty t.lhs)
            (arm_operands_pretty op_tags t.operands))

(* TODO: print the tid *)
let arm_blk_pretty (t : IR.blk) : (string list, Errors.t) result =
  List.map ~f:arm_op_pretty t.operations |> Result.all

let arm_ir_pretty (t : IR.t) : (string list, Errors.t) result =
  List.map ~f:arm_blk_pretty t.blks |> Result.all |> Result.map ~f:List.concat

