open !Core_kernel
open Bap.Std
open Bap_core_theory

type arm_eff = {
  current_data : Ir.operation list;
  current_ctrl : Ir.operation list;
  other_blks : Ir.t}
[@@deriving compare, equal, sexp]

let empty_eff = {current_data = []; current_ctrl = []; other_blks = Ir.empty}

type arm_pure = {op_val : Ir.operand; op_eff : arm_eff}
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
  let { current_data = data1; current_ctrl = ctrl1; other_blks = blks1} = s1 in
  let { current_data = data2; current_ctrl = ctrl2; other_blks = blks2} = s2 in
  {
    current_data = data1 @ data2;
    current_ctrl = ctrl1 @ ctrl2;
    other_blks = Ir.union blks1 blks2
  }

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

  module Ops = struct

    let op s = Ir.Opcode.create ~arch:"arm" s

    let mov = op "mov"
    (* let movw = op "movw" *)
    (* let bx = op "bx" *)
    let add = op "add"
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
    let cmp = op "comp"
    (* let beq = op "beq" *)
    let bne = op "bne"
    (* let ble = op "ble" *)
    (* let blt = op "blt" *)
    let b = op "b"

  end

  let create_temp ty =
    Var.create ~is_virtual:true ~fresh:true "tmp" ty |>
    Ir.simple_var

  (* let create_temp' reg =
   *   let v = Var.create ~is_virtual:true ~fresh:true "tmp" (Imm 32) in
   *   Ir.given_var v reg *)

  (* defaults to data instructions, since they are way more common *)
  let instr i sem =
    {sem with current_data = i::sem.current_data}

  let control j sem =
    {sem with current_ctrl = j::sem.current_ctrl}

  (* Some cowboy type checking here, to check which kind of mov to
     use. Currently doesn't work if variables are instantiated
     with spilled registers! Can be fixed by having seperate Variable
     constructors for spilled and non-spilled registers. *)
  let arm_mov arg1 arg2 =
    let {op_val = arg2_var; op_eff = arg2_sem} = arg2 in
    let mov =
      match arg1, arg2_var with
      | Ir.Var _, Ir.Var _
      | Ir.Var _, Ir.Const _ ->
        Ir.simple_op Ops.mov arg1 [arg2_var]
      | _ -> failwith "arm_mov: unexpected arguments!"
    in
    instr mov arg2_sem

  let ( := ) x y = arm_mov x y

  let b_instr addr =
    let i = Ir.simple_op Ops.b Void [Ir.Label addr] in
    control i empty_eff

  let jmp arg =
    let {op_val = arg_tgt; op_eff = arg_sem} = arg in
    let pc = Var.create "PC" (Imm 32) in
    let pc = Ir.Var (Ir.simple_var pc) in
    let jmp_data, jmp_ctrl =
      match arg_tgt with
      | Var _ ->
        [], Ir.simple_op Ops.mov pc [arg_tgt]
      | Const w ->
        [], Ir.simple_op Ops.b Void [Offset w]
      | _ ->
        let err = Format.asprintf "%s"
            (Ir.sexp_of_operand arg_tgt |>
             Sexp.to_string)
        in
        failwith @@ "jmp: unexpected operand " ^ err
    in
    {
      arg_sem with
      current_data = jmp_data @ arg_sem.current_data;
      current_ctrl = jmp_ctrl::arg_sem.current_ctrl
    }


  let var v = {op_val = Ir.Var (Ir.simple_var v); op_eff = empty_eff}

  let const c = {op_val = Ir.Const c; op_eff = empty_eff}

  let uop o ty arg =
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

  let (+) arg1 arg2 = binop Ops.add (Imm 32) arg1 arg2

  let (-) arg1 arg2 = binop Ops.sub (Imm 32) arg1 arg2

  let shl _signed arg1 arg2 = binop Ops.lsl_ (Imm 32) arg1 arg2

  let shr signed arg1 arg2 =
    let b =
      match signed.op_val with
      | Ir.Const w -> (Word.to_int_exn w) <> 0
      (* FIXME: Not sure what to do here; generally shifts are done by
         constant amounts, and at any rate it requires a bit of work
         to implement in ARM. Most likely the right thing to do is
         fail gracefully.  *)
      | _ -> failwith "Arm_gen.shr: arg2 non-constant"
    in
    if b then
      binop Ops.asr_ (Imm 32) arg1 arg2
    else
      binop Ops.lsr_ (Imm 32) arg1 arg2

  let ldr bits mem loc =
    (* Update the semantics of loc with those of mem *)
    let loc = {loc with op_eff = loc.op_eff @. mem.op_eff} in
    let l_instr =
      if bits = 32 then
        Ops.ldr
      else if bits = 16 then
        Ops.ldrh
      else if bits = 8 then
        Ops.ldrb
      else
        failwith "Arm_selector.ldr: Loading a bit-width that is not 8, 16 or 32!"
    in
    uop l_instr (Imm 32) loc

  let ldr32 mem loc = ldr 32 mem loc

  let str mem value loc =
    let {op_val = loc_val; op_eff = loc_sem} = loc in
    let {op_val = value_val; op_eff = value_sem} = value in
    let {op_val = mem_val; op_eff = mem_sem} = mem in
    let op = Ir.simple_op Ops.str loc_val [value_val] in
    (* Again, a little cowboy instruction ordering *)
    let sem = loc_sem @. value_sem @. mem_sem in
    let sem = {sem with current_data = op::sem.current_data} in
    {op_val = mem_val; op_eff = sem}

  let (&&) a b = binop Ops.and_ (Imm 32) a b

  let (||) a b = binop Ops.orr (Imm 32) a b

  let xor a b = binop Ops.eor (Imm 32) a b

  (* Generally, boolean operations will be handled by a normal (word
     size) value, with value 0 if false and non-zero if true. It will
     be the job of branching operations to call [cmp ? ?] and check
     the appropriate flags. *)
  let equals arg1 arg2 = binop Ops.sub (Imm 32) arg1 arg2

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
    let cmp = Ir.simple_op Ops.cmp Void
        [cond_val; Const (Word.of_int ~width:32 0)] in
    let {current_data = data1; current_ctrl = ctrl1; other_blks = blks1} = branch1 in
    let {current_data = data2; current_ctrl = ctrl2; other_blks = blks2} = branch2 in
    let tid1 = Tid.for_name "true_branch" in
    let tid2 = Tid.for_name "false_branch" in
    let blk1 = Ir.simple_blk tid1 ~data:(List.rev data1) ~ctrl:ctrl1 in
    let blk2 = Ir.simple_blk tid2 ~data:(List.rev data2) ~ctrl:ctrl2 in
    let beq = Ir.simple_op Ops.bne Void [Label tid2] in
    let blks = cond_eff.other_blks in
    let blks = Ir.union blks1 @@ Ir.union blks2 blks in
    let blks = Ir.add blk1 @@ Ir.add blk2 blks in
    {
      current_data = cmp :: cond_eff.current_data;
      current_ctrl = [beq];
      other_blks = blks
    }


end


module ARM_Core : Theory.Core =
struct
  include Theory.Empty
  include ARM_ops

  let set v arg =
    let arg_v =
      let r_var = v |> Var.reify in
      Ir.simple_var r_var
    in
    KB.(
      arg >>= fun arg ->
      match Value.get arm_pure arg with
      | Some arg -> eff ((Ir.Var arg_v) := arg)
      | None ->
        begin
          match Value.get arm_mem arg with
          (* No need to explicitely assign here, we don't use the
             "mem" variables when generating Ir. *)
          | Some arg -> eff arg.op_eff
          | None -> assert false
        end)


  let seq s1 s2 =
    let= s1 = s1 in
    let= s2 = s2 in
    eff @@ s1 @. s2

  (* Both [data] and [ctrl] effects can have both kinds of effects,
     AFAIKT, so we treat them (almost) identically. *)
  let blk lab data ctrl =
    let= data = data in
    let= ctrl = ctrl in
    (* We add instructions by consing to the front of the list, so we
       need to reverse before finalizing the block. *)
    let new_data = ctrl.current_data @ data.current_data |> List.rev in
    (* Currently we generate ctrl in the correct order, since
       there are rougly only 2 or 3 instructions. *)
    let new_ctrl = ctrl.current_ctrl @ data.current_ctrl in
    let new_blk = Ir.simple_blk lab ~data:new_data ~ctrl:new_ctrl in
    let all_blocks =
      Ir.add new_blk @@ Ir.union data.other_blks ctrl.other_blks in
    eff {current_data = []; current_ctrl = []; other_blks = all_blocks}

  let var (v : 'a Theory.var) : 'a Theory.pure =
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
    (* FIXME: we're assuming every constant is exactly 32
       bits. *)
    let w = Bitvec.to_int32 w in
    pure @@ const @@ Word.of_int32 ~width:32 w

  let add a b =
    let- a = a in
    let- b = b in
    pure @@ a + b

  let sub a b =
    let- a = a in
    let- b = b in
    pure @@ a - b

  let goto (lab : tid) : Theory.ctrl Theory.eff =
    eff @@ b_instr lab

  let jmp addr =
    let- addr_bitv = addr in
    eff @@ jmp addr_bitv

  let repeat _cond _body =
    Errors.fail (Errors.Not_implemented "Arm_gen.repeat")

  let load mem loc =
    let/ mem = mem in
    let- loc = loc in
    pure @@ ldr32 mem loc

  (* FIXME: check the endian is always false? *)
  let loadw sort _endian mem loc =
    let/ mem = mem in
    let- loc = loc in
    let size = Theory.Bitv.size sort in
    pure @@ ldr size mem loc

  let store mem loc value =
    let/ mem = mem in
    let- loc = loc in
    let- value = value in
    (* We have to swap the arguments here, since ARM likes the value
       first, and the location second *)
    memory @@ str mem value loc

  let perform _sort =
    eff empty_eff

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

  let eq a b =
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

let add_in_vars_blk (b : Ir.blk) : Ir.blk =
  (* We only grab data here, since control effects don't influence
     dependencies. *)
  let ops = b.data in
  let add_list set l = List.fold l ~init:set ~f:Var.Set.add in
  let collect_vars_op_list l =
    List.fold l ~init:Var.Set.empty
      ~f:(fun set o ->
          match o with
          | Ir.Var v -> add_list set v.temps
          | _ -> set)
  in
  (* Simultaneously collect defined and undefined vars *)
  let _, undefined =
    List.fold ops ~init:(Var.Set.empty, Var.Set.empty)
      ~f:(fun (defined, undefined) o ->
          let undef = collect_vars_op_list o.operands in
          let undef = Var.Set.diff undef defined in
          let def = collect_vars_op_list o.lhs in
          Var.Set.union defined def, Var.Set.union undefined undef)
  in
  let ins = Var.Set.fold undefined ~init:[]
      ~f:(fun ins v -> Ir.Var (Ir.simple_var v)::ins)
  in
  (* We add dummy operation with no instructions and as lhs all the
     [ins] variables *)
  let ins = {
    Ir.id = Tid.create ();
    Ir.lhs = ins;
    Ir.opcodes = [];
    Ir.optional = false;
    Ir.operands = [];
  } in
  {b with ins = ins}

(* Collect all the variables appearing on rhs that are not defined by
   an rhs before-hand. *)
let add_in_vars (t : Ir.t) : Ir.t =
  { t with blks = List.map ~f:add_in_vars_blk t.blks }

(* We assume that a block is always created *)
let ir (t : arm_eff) : Ir.t =
  assert Core_kernel.(List.is_empty t.current_data
                      && List.is_empty t.current_ctrl);
  let blks = t.other_blks in
  add_in_vars blks


module Pretty = struct

  let opcode_pretty i : (string, Errors.t) result = Result.return @@ Ir.Opcode.name i

  (* We use this function when generating ARM, since the assembler
     doesn't like % or @ in labels. *)
  let tid_to_string (t : tid) : string =
    Tid.name t |> String.strip ~drop:Char.(fun c -> c = '%' || c = '@')

  let arm_operand_pretty ~is_loc:is_loc (o : Ir.operand) : (string, Errors.t) result =
    match o with
    | Var v ->
      let error =
        Errors.Missing_semantics
          "arm_operand_pretty: operand.pre_assign field is empty" in
      let res =
        Result.bind
          (Result.of_option v.pre_assign ~error:error)
          ~f:(fun reg -> Result.return @@ Var.to_string reg)
      in
      if is_loc then
        Result.map res ~f:(fun s -> Format.asprintf "[%s]" s)
      else
        res
    | Const w ->
      (* A little calisthenics to get this to look nice *)
      Result.return @@ Format.asprintf "#%a" Word.pp_dec w
    | Label l -> Result.return @@ tid_to_string l
    | Void -> Result.return ""
    | Offset c ->
      (* Special printing of offsets to jump back from patched locations *)
      Result.return @@
      Format.asprintf "(%s + %d - %s)" Constants.patch_start_label (Word.to_int_exn c) Constants.relative_patch_placement


  (* FIXME: Absolute hack *)
  let mk_loc_list (op : string) (args : 'a list) : bool list =
    if String.(op = "ldr" || op = "ldrh" || op = "ldrb") then
      [false; true]
    else
      List.init (List.length args) ~f:(fun _ -> false)

  let arm_operands_pretty (op : string) (hd : Ir.operand) (l : Ir.operand list)
    : (string, Errors.t) result =
    (* Don't print the head of the operand if it's void. *)
    let l =
      match hd with
      | Void -> l
      | _ -> hd::l
    in
    let is_loc_list = mk_loc_list op l in
    let l = List.zip_exn is_loc_list l in
    let all_str =
      List.map l
        ~f:(fun (is_loc, o) -> arm_operand_pretty ~is_loc:is_loc o) |>
      Result.all
    in
    let all_str = Result.map ~f:(List.intersperse ~sep:", ") all_str in
    Result.map ~f:String.concat all_str

  let arm_op_pretty (t : Ir.operation) : (string, Errors.t) result =
    let op = List.hd_exn t.opcodes in
    Result.(opcode_pretty op >>= fun op ->
            arm_operands_pretty op
              (List.hd_exn t.lhs)
              t.operands >>= (fun operands ->
                  return (Format.asprintf "%s %s" op operands)))

  let arm_blk_pretty (t : Ir.blk) : (string list, Errors.t) result =
    let all_ops = t.data @ t.ctrl in
    let opcodes = List.map ~f:arm_op_pretty all_ops |> Result.all in
    let lab = Format.asprintf "%s:" (tid_to_string t.id) in
    Result.map opcodes ~f:(fun opcodes -> lab::opcodes)

  let arm_ir_pretty (t : Ir.t) : (string list, Errors.t) result =
    List.map ~f:arm_blk_pretty t.blks |> Result.all |> Result.map ~f:List.concat

end

let slot = arm_eff

let () =
  Theory.declare
    ~context:["vibes"]
    ~package:"vibes"
    ~name:"arm-gen"
    ~desc:"This theory allows instantiating Program semantics into \
          a Vibes_ir.t term, using Arm_gen.slot."
  @@ KB.return (module ARM_Core : Theory.Core)
