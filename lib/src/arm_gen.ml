open !Core_kernel
open Bap.Std
open Bap_core_theory

module IR = Vibes_ir

type arm_eff = {current_blk : IR.operation list; other_blks : IR.t}
[@@deriving compare, equal, sexp]

let empty_eff = {current_blk = []; other_blks = IR.empty}

(* FIXME: if this is a constant, I'm pretty sure the op_eff field is
   always empty *)
type arm_pure = {op_val : IR.operand; op_eff : arm_eff}
[@@deriving compare, equal, sexp]

(* We use this domain both for ['a pure] and ['s bitv] *)
let arm_pure_domain =
  KB.Domain.optional
    ~inspect:sexp_of_arm_pure
    ~equal:equal_arm_pure "arm-pure"

let arm_pure =
  KB.Class.property Theory.Value.cls "arm-pure" arm_pure_domain

let reify_var (v : 'a Theory.var) : Bil.var = Var.reify v

let (@.) s1 s2 =
  let { current_blk = blk1; other_blks = blks1} = s1 in
  let { current_blk = blk2; other_blks = blks2} = s2 in
  {current_blk = blk1 @ blk2; other_blks = IR.union blks1 blks2}

let arm_eff_domain =
  KB.Domain.optional
    ~inspect:sexp_of_arm_eff
    ~equal:equal_arm_eff
    "arm-eff"

let arm_eff = KB.Class.property Theory.Effect.cls "arm-eff" arm_eff_domain

let effect v = KB.Value.get arm_eff v

let arm_mem = KB.Class.property Theory.Value.cls "arm-mem" arm_pure_domain

let (let=) v f = KB.(>>=) v
    (fun v ->
       match KB.Value.get arm_eff v with
       | None ->
         let v_str = Format.asprintf "arm_eff: %a" KB.Value.pp v in
         Errors.fail (Errors.Missing_semantics v_str)
       | Some v -> f v)

let (let-) v f =
  KB.(>>=) v
    (fun v ->
       match KB.Value.get arm_pure v with
       | None ->
         let v_str = Format.asprintf "arm_pure: %a" KB.Value.pp v in
         Errors.fail (Errors.Missing_semantics v_str)
       | Some v -> f v)

let (let/) v f =
  KB.(>>=) v
    (fun v ->
       match KB.Value.get arm_mem v with
       | None ->
         let v_str = Format.asprintf "arm_mem: %a" KB.Value.pp v in
         Errors.fail (Errors.Missing_semantics v_str)
       | Some v -> f v)

let eff d =
  KB.return @@
  KB.Value.put arm_eff
    (Theory.Effect.empty Theory.Effect.Sort.bot)
    (Some d)

type 'a bitv_sort = 'a Theory.Bitv.t Theory.Value.sort

(* We make this polymorphic, so that it can be instantiated in any
   setting, despite being a fixed given size. We add a [unit] argument
   to avoid the value restriction. *)
(* TODO: fix this, so that s32 is of type r32 sort *)
let s32 (_ : unit) : 'a bitv_sort = Theory.Bitv.define 32

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

  let freshen_operand o =
    match o with
    | IR.Var v ->
      let fresh_v =
        { v with
          id =
            Var.create
              ~is_virtual:true
              ~fresh:true
              (Var.name v.id)
              (Var.typ v.id)
        } in
      IR.Var fresh_v
    | _ -> o

  let instr i sem =
    {current_blk = i::sem.current_blk; other_blks = sem.other_blks}

  (* Some cowboy type checking here, to check which kind of mov to use.
     FIXME: certainly doesn't work if variables are instantiated with
     immediates! Can be fixed by having seperate Variable constructors
     for registers and immediates. *)
  let arm_mov arg1 arg2 =
    let {op_val = arg2_var; op_eff = arg2_sem} = arg2 in
    let arg2_var = freshen_operand arg2_var in
    let mov =
      match arg1, arg2_var with
      | IR.Var _, IR.Var _ ->
        IR.simple_op `MOVr arg1 [arg2_var]
      | IR.Var _, IR.Const _ -> IR.simple_op `MOVi arg1 [arg2_var]
      | _ -> failwith "arm_mov: unexpected arguments!"
    in
    instr mov arg2_sem

  let ( := ) x y = arm_mov x y

  let b addr =
    let i = IR.simple_op `Bcc Void [IR.Label addr] in
    instr i empty_eff

  (* TODO: only works for registers? *)
  let jmp arg =
    let {op_val = arg_const; op_eff = arg_sem} = arg in
    let jmp =
      match arg_const with
      | Var _ | Label _ -> IR.simple_op `BX Void [arg_const]
      | _ ->
        let err = Format.asprintf "%s"
            (IR.sexp_of_operand arg_const |>
             Sexp.to_string)
        in
        failwith @@ "jmp: unexpected operand " ^ err
    in
    instr jmp arg_sem

  let var v = {op_val = IR.Var (IR.simple_var v); op_eff = empty_eff}

  let const c = {op_val = IR.Const c; op_eff = empty_eff}

  let uop o ty arg =
    let res = Var.create ~is_virtual:true ~fresh:true "temp" ty |> IR.simple_var in
    let {op_val = arg_val; op_eff = arg_sem} = arg in
    let arg_val = freshen_operand arg_val in
    let op = IR.simple_op o (IR.Var res) [arg_val] in
    let sem = {arg_sem with current_blk = op::arg_sem.current_blk} in
    {op_val = IR.Var res; op_eff = sem}

  let binop o ty arg1 arg2 =
    let res = Var.create ~is_virtual:true ~fresh:true "temp" ty |> IR.simple_var in
    let {op_val = arg1_val; op_eff = arg1_sem} = arg1 in
    let {op_val = arg2_val; op_eff = arg2_sem} = arg2 in
    let arg1_val = freshen_operand arg1_val in
    let arg2_val = freshen_operand arg2_val in
    let op = IR.simple_op o (IR.Var res) [arg1_val; arg2_val] in
    let sem = arg1_sem @. arg2_sem in
    let sem = {sem with current_blk = op::sem.current_blk} in
    {op_val = IR.Var res; op_eff = sem}

  (* TODO: handle non-register arguments? *)
  let (+) arg1 arg2 = binop `ADDrsi (Imm 32) arg1 arg2

  let (-) arg1 arg2 = binop `SUBrsi (Imm 32) arg1 arg2

  let shl _signed arg1 arg2 = binop `LSL (Imm 32) arg1 arg2

  let shr signed arg1 arg2 =
    let b =
      match signed.op_val with
      | IR.Const w -> (Word.to_int_exn w) <> 0
      (* FIXME: Not sure what to do here; generally shifts are done by
         constant amounts, and at any rate it requires a bit of work
         to implement in ARM. Most likely the right thing to do is
         fail gracefully.  *)
      | _ -> assert false
    in
    if b then
      binop `ASR (Imm 32) arg1 arg2
    else
      binop `LSR (Imm 32) arg1 arg2

  let ldr mem loc =
    (* Update the semantics of loc with those of mem *)
    let loc = {loc with op_eff = loc.op_eff @. mem.op_eff} in
    uop `LDRrs (Imm 32) loc

  let str mem value loc =
    let {op_val = loc_val; op_eff = loc_sem} = loc in
    let {op_val = value_val; op_eff = value_sem} = value in
    let {op_val = mem_val; op_eff = mem_sem} = mem in
    let op = IR.simple_op `STRrs loc_val [value_val] in
    (* Again, a little cowboy instruction ordering *)
    let sem = loc_sem @. value_sem @. mem_sem in
    let sem = {sem with current_blk = op::sem.current_blk} in
    {op_val = mem_val; op_eff = sem}

  let (&&) a b = binop `ANDrsi (Imm 32) a b

  let (||) a b = binop `ORRrsi (Imm 32) a b

  let xor a b = binop `EORrsi (Imm 32) a b

  (* Generally, boolean operations will be handled by a normal (word
     size) value, with value 0 if false and non-zero if true. It will
     be the job of branching operations to call [cmp ? ?] and check
     the appropriate flags. *)
  let equals arg1 arg2 = binop `SUBrsi (Imm 32) arg1 arg2

  (* Intuitively we want to generate:

     ..cond.. //fills variable [cond_val]
     CMP cond_val 0
     BEQ
     ..branch1... //this should just be a single jump instruction
     ..branch2... //same, but less important

  *)
  let beq cond branch1 branch2 =
    let {op_val = cond_val; op_eff = cond_eff} = cond in
    (* the order of operations here is actually important! *)
    let cond_val = freshen_operand cond_val in
    let cmp = IR.simple_op `CMPrsi Void [cond_val; Const (Word.of_int ~width:32 0)] in
    let beq = IR.simple_op `Bcc Void [Cond `EQ] in
    let {current_blk = blk1; other_blks = blks1} = branch1 in
    let {current_blk = blk2; other_blks = blks2} = branch2 in
    {
      current_blk =
          blk2 @
          blk1 @
          beq::cmp::
          cond_eff.current_blk;
      other_blks = IR.union blks1 @@ IR.union blks2 cond_eff.other_blks
    }


end


module ARM_Core : Theory.Core =
struct
  include Theory.Empty
  include ARM_ops

  let set v arg =
    Events.(send @@ Info "calling set");
    let arg_v =
      let r_var = v |> Var.reify in
      IR.simple_var r_var
    in
    KB.(
      arg >>= fun arg ->
      match Value.get arm_pure arg with
      (* FIXME: freshen the lhs? *)
      | Some arg -> eff ((IR.Var arg_v) := arg)
      | None ->
        begin
          match Value.get arm_mem arg with
          (* No need to explicitely assign here, we don't use the
             "mem" variables when generating IR. *)
          | Some arg -> eff arg.op_eff
          | None -> assert false
        end)


  let seq s1 s2 =
    Events.(send @@ Info "calling seq");
    let= s1 = s1 in
    let= s2 = s2 in
    eff @@ s1 @. s2

  let blk lab data ctrl =
    Events.(send @@ Info "calling blk");
    let= data = data in
    let= ctrl = ctrl in
    let new_instrs = ctrl.current_blk @ data.current_blk in
    (* We add instructions by consing to the front of the list, so we
       need to reverse before finalizing the block *)
    let new_instrs = List.rev new_instrs in
    let new_blk = IR.simple_blk lab new_instrs in
    let all_blocks =
      IR.add new_blk @@ IR.union data.other_blks ctrl.other_blks in
    eff {current_blk = []; other_blks = all_blocks}

  let var (v : 'a Theory.var) : 'a Theory.pure =
    Events.(send @@ Info "calling var");
    let sort = Theory.Var.sort v in
    let v = reify_var v in
    let slot =
      match Theory.Value.Sort.forget sort |> Theory.Mem.refine with
      | None -> arm_pure
      | Some _ -> arm_mem
    in
    KB.return @@ KB.Value.put slot (Theory.Value.empty sort)
      (Some (var v))

  let unk _sort = Errors.fail (Errors.Not_implemented "Arm_gen.unk")

  let let_ _v _e _b = Errors.fail (Errors.Not_implemented "Arm_gen.let_")

  let int _sort (w : Theory.word) : 's Theory.bitv =
    Events.(send @@ Info "calling int");
    (* FIXME: we're assuming every constant is exactly 32
       bits. *)
    let w = Bitvec.to_int32 w in
    pure @@ const @@ Word.of_int32 ~width:32 w

  let add a b =
    Events.(send @@ Info "calling add");
    let- a = a in
    let- b = b in
    pure @@ a + b

  let sub a b =
    Events.(send @@ Info "calling sub");
    let- a = a in
    let- b = b in
    pure @@ a - b

  let goto (lab : tid) : Theory.ctrl Theory.eff =
    Events.(send @@ Info "calling goto");
    eff @@ b lab

  let jmp addr =
    Events.(send @@ Info "calling jmp");
    let- addr_bitv = addr in
    eff @@ jmp addr_bitv

  let repeat _cond _body =
    Errors.fail (Errors.Not_implemented "Arm_gen.repeat")

  let load mem loc =
    Events.(send @@ Info "calling load");
    let/ mem = mem in
    let- loc = loc in
    pure @@ ldr mem loc

  let store mem loc value =
    Events.(send @@ Info "calling store");
    let/ mem = mem in
    let- loc = loc in
    let- value = value in
    (* We have to swap the arguments here, since ARM likes the value
       first, and the location second *)
    memory @@ str mem value loc

  let perform _sort =
    Events.(send @@ Info "calling perform");
    eff {current_blk = []; other_blks = IR.empty}

  let shiftl sign l r =
    Events.(send @@ Info "calling shiftl");
    let- sign = sign in
    let- l = l in
    let- r = r in
    pure @@ shl sign l r

  let shiftr sign l r =
    Events.(send @@ Info "calling shiftr");
    let- sign = sign in
    let- l = l in
    let- r = r in
    pure @@ shr sign l r

  let and_ a b =
    Events.(send @@ Info "calling and_");
    let- a = a in
    let- b = b in
    bool (a && b)

  let or_ a b =
    Events.(send @@ Info "calling or_");
    let- a = a in
    let- b = b in
    bool (a || b)

  let logand a b =
    Events.(send @@ Info "calling logand");
    let- a = a in
    let- b = b in
    pure (a && b)

  let logor a b =
    Events.(send @@ Info "calling logor");
    let- a = a in
    let- b = b in
    pure (a || b)

  let logxor a b =
    Events.(send @@ Info "calling logxor");
    let- a = a in
    let- b = b in
    pure @@ xor a b

  let eq a b =
    Events.(send @@ Info "calling eq");
    let- a = a in
    let- b = b in
    bool @@ equals a b

  let b0 = bool @@ const @@ Word.of_int ~width:32 0

  let b1 = bool @@ const @@ Word.of_int ~width:32 1

  let branch cond branch1 branch2 =
    let- cond = cond in
    let= branch1 = branch1 in
    let= branch2 = branch2 in
    eff @@ beq cond branch1 branch2

end

let add_in_vars_blk (b : IR.blk) : IR.blk =
  let ops = b.operations in
  let add_list set l = List.fold l ~init:set ~f:Var.Set.add in
  let collect_vars_op_list l =
    List.fold l ~init:Var.Set.empty
      ~f:(fun set o ->
          match o with
          | IR.Var v -> add_list set v.temps
          | _ -> set)
  in
  (* Simultaneously collect defined and undefined vars *)
  let _, undefined =
    List.fold ops ~init:(Var.Set.empty, Var.Set.empty)
      ~f:(fun (defined, undefined) o ->
          let undef = collect_vars_op_list o.operands in
          let undef = Var.Set.diff defined undef in
          let def = collect_vars_op_list o.lhs in
          Var.Set.union defined def, Var.Set.union undefined undef)
  in
  let ins = Var.Set.fold undefined ~init:[]
      ~f:(fun ins v -> IR.Var (IR.simple_var v)::ins)
  in
  (* We add dummy operation `HINT and lhs Void *)
  let ins = IR.simple_op `HINT IR.Void ins in
  {b with ins = ins}

(* Collect all the variables appearing on rhs that are not defined by
   an rhs before-hand. *)
let add_in_vars (t : IR.t) : IR.t =
  { t with blks = List.map ~f:add_in_vars_blk t.blks }

(* FIXME: not sure what the right behavior is here... should we assume that a
   block is always created? *)
let ir (t : arm_eff) : IR.t =
  assert (List.is_empty t.current_blk);
  let blks = t.other_blks in
  add_in_vars blks

let insn_pretty i : (string, Errors.t) result =
  match i with
  | `MOVr
  | `MOVi   -> Ok "mov"
  | `BX     -> Ok "bx"
  | `ADDrsi -> Ok "add"
  | `SUBrsi -> Ok "sub"
  | `LSL    -> Ok "lsl"
  | `LSR    -> Ok "lsr"
  | `ASR    -> Ok "asr"
  | `ANDrsi -> Ok "and"
  | `ORRrsi -> Ok "orr"
  | `EORrsi -> Ok "eor"
  | `LDRrs  -> Ok "ldr"
  | `STRrs  -> Ok "str"
  | `Bcc    -> Ok "bcc"
  | `CMPrsi -> Ok "cmp"
  | i       ->
    let to_string i = IR.sexp_of_insn i |> Sexp.to_string in
    let msg =
      Format.asprintf "insn_pretty: instruction %s not supported" (to_string i)
    in
    Error (Errors.Not_implemented msg)

(* We use this function when generating ARM, since the assembler
   doesn't like % or @ in labels. *)
let tid_to_string (t : tid) : string =
  Tid.name t |> String.strip ~drop:Char.(fun c -> c = '%' || c = '@')

let arm_operand_pretty (o : IR.operand) : (string, Errors.t) result =
  match o with
  | Var v ->
     let error =
       Errors.Missing_semantics
         "arm_operand_pretty: operand.pre_assign field is empty" in
     Result.bind
       (Result.of_option v.pre_assign ~error:error)
       ~f:(fun reg ->
         Result.return @@ Sexp.to_string @@ ARM.sexp_of_gpr_reg reg)
  | Const w ->
    (* A little calisthenics to get this to look nice *)
    Result.return @@ Format.asprintf "#%a" Word.pp_dec w
  | Label l -> Result.return @@ tid_to_string l
  | Cond c -> Result.return @@ IR.cond_to_string c
  | Void -> Result.return ""

let arm_operands_pretty (hd : IR.operand) (l : IR.operand list)
  : (string, Errors.t) result =
  (* Don't print the head of the operand if it's void. *)
  let l =
    match hd with
    | Void -> l
    | _ -> hd::l
  in
    Result.map ~f:(String.concat ~sep:", ")
      (Result.all (List.map l ~f:(fun o -> arm_operand_pretty o)))

let arm_op_pretty (t : IR.operation) : (string, Errors.t) result =
  let op = List.hd_exn t.insns in
  Result.(insn_pretty op >>= fun op ->
          arm_operands_pretty (List.hd_exn t.lhs) t.operands >>= (fun operands ->
              return (Format.asprintf "%s %s" op operands)))

(* TODO: print the tid *)
let arm_blk_pretty (t : IR.blk) : (string list, Errors.t) result =
  let insns = List.map ~f:arm_op_pretty t.operations |> Result.all in
  let lab = Format.asprintf "%s:" (tid_to_string t.id) in
  Result.map insns ~f:(fun insns -> lab::insns)

let arm_ir_pretty (t : IR.t) : (string list, Errors.t) result =
  List.map ~f:arm_blk_pretty t.blks |> Result.all |> Result.map ~f:List.concat


let slot = arm_eff

let () =
  Theory.declare
    ~context:["vibes"]
    ~package:"vibes"
    ~name:"arm-gen"
    ~desc:"This theory allows instantiating Program semantics into \
          a Vibes_ir.t term, using Arm_gen.slot."
  @@ KB.return (module ARM_Core : Theory.Core)
