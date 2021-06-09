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

let is_thumb (lang : Theory.language) : bool =
  let l = Theory.Language.to_string lang in
  if String.is_substring l ~substring:"arm" then
    false
  else if String.is_substring l ~substring:"thumb" then
    true
  else
    let err =
      Format.asprintf "is_thumb: unsupported language: %s" l
    in
    failwith err

let gpr (tgt : Theory.target) (lang : Theory.language) =
  let roles = [Theory.Role.Register.general] in
  let roles =
    if not (Theory.Language.is_unknown lang) &&
       is_thumb lang
    then
      Theory.Role.read ~package:"arm" "thumb"::roles
    else roles
  in
  let maybe_reify v =
    let v = Var.reify v in
    let name = Var.name v in
    if String.(is_prefix name ~prefix:"R") then
      Some v
    else None
  in
  Theory.Target.regs ~roles:roles tgt |>
  Set.filter_map ~f:(maybe_reify) (module Var)

let preassign_var (lang : Theory.language) (v : var) : var option =
  if String.(Var.name v = "FP") then
    begin
      (* We assign R11 as the pre-assigned FP register on ARM, and R7
         for Thumb, keeping in line with the ABI (as far as i can
         tell).  *)
      if Theory.Language.is_unknown lang then None
      else if is_thumb lang then
        Some (Var.create ~is_virtual:false ~fresh:false "R7" (Var.typ v))
      else
        Some (Var.create ~is_virtual:false ~fresh:false "R11" (Var.typ v))
    end
  else if String.(Var.name v = "PC") then
    Some (Var.create ~is_virtual:false ~fresh:false "PC" (Var.typ v))
  else
    None

let preassign (lang : Theory.language) (ir : Ir.t) : Ir.t =
    Ir.map_op_vars ir
      ~f:(fun v -> {v with pre_assign = List.hd_exn v.temps |> preassign_var lang})

let (@.) s1 s2 =
  let { current_data = data1; current_ctrl = ctrl1; other_blks = blks1} = s1 in
  let { current_data = data2; current_ctrl = ctrl2; other_blks = blks2} = s2 in
  {
    current_data = data1 @ data2;
    current_ctrl = ctrl1 @ ctrl2;
    other_blks = Ir.union blks1 blks2
  }

let (@>) eff data =
  { data with op_eff = eff @. data.op_eff }

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
         Kb_error.fail (Kb_error.Missing_semantics v_str)
       | Some v -> f v)

let (let-) v f =
  KB.(>>=) v
    (fun v ->
       match KB.Value.get arm_pure v with
       | None ->
         let v_str = Format.asprintf "arm_pure: %a" KB.Value.pp v in
         Kb_error.fail (Kb_error.Missing_semantics v_str)
       | Some v -> f v)

let (let/) v f =
  KB.(>>=) v
    (fun v ->
       match KB.Value.get arm_mem v with
       | None ->
         let v_str = Format.asprintf "arm_mem: %a" KB.Value.pp v in
         Kb_error.fail (Kb_error.Missing_semantics v_str)
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

    include Ir.Opcode

    let op s = create ~arch:"arm" s

    let mov = op "mov"
    (* let movw = op "movw" *)
    (* let bx = op "bx" *)
    let add = op "add"
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
    let beq = op "beq"
    let sdiv = op "sdiv"
    let udiv = op "udiv"
    (* let bne = op "bne" *)
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

  let ( * ) arg1 arg2 = binop Ops.mul (Imm 32) arg1 arg2

  let (-) arg1 arg2 = binop Ops.sub (Imm 32) arg1 arg2

  let (/) arg1 arg2 = binop Ops.sdiv (Imm 32) arg1 arg2

  let udiv arg1 arg2 = binop Ops.udiv (Imm 32) arg1 arg2

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

  let ldr_op bits =
    if bits = 32 then
      Ops.ldr
    else if bits = 16 then
      Ops.ldrh
    else if bits = 8 then
      Ops.ldrb
    else
      failwith "Arm_selector.ldr: Loading a bit-width that is not 8, 16 or 32!"

  let ldr bits mem loc =
    (* Update the semantics of loc with those of mem *)
    let loc = {loc with op_eff = loc.op_eff @. mem.op_eff} in
    uop (ldr_op bits) (Imm 32) loc

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
    let beq = Ir.simple_op Ops.beq Void [Label tid1] in
    let b = Ir.simple_op Ops.b Void [Label tid2] in
    let blks = cond_eff.other_blks in
    let blks = Ir.union blks1 @@ Ir.union blks2 blks in
    let blks = Ir.add blk1 @@ Ir.add blk2 blks in
    {
      current_data = cmp :: cond_eff.current_data;
      current_ctrl = [beq; b];
      other_blks = blks
    }

  (* A simpler branch which takes in tids *)
  let br cond tgt alt =
    let {op_val = cond_val; op_eff = cond_eff} = cond in
    (* the order of operations here is actually important! *)
    let cmp = Ir.simple_op Ops.cmp Void
        [cond_val; Const (Word.of_int ~width:32 0)] in
    let beq = Ir.simple_op Ops.beq Void [Label tgt] in
    let b = Ir.simple_op Ops.b Void [Label alt] in
    let blks = cond_eff.other_blks in
    {
      current_data = cmp :: cond_eff.current_data;
      current_ctrl = [beq; b];
      other_blks = blks
    }


end

module ARM_Gen =
struct
  open ARM_ops

  let sel_binop (o : binop) : arm_pure -> arm_pure -> arm_pure =
    match o with
    | PLUS -> (+)
    | MINUS -> (-)
    | TIMES -> ( * )
    | DIVIDE -> udiv
    | SDIVIDE -> (/)
    | LSHIFT -> (binop Ops.lsl_ (Imm 32))
    | RSHIFT -> (binop Ops.lsr_ (Imm 32))
    | ARSHIFT -> (binop Ops.asr_ (Imm 32))
    | AND -> (&&)
    | OR -> (||)
    | XOR -> xor
    | MOD
    | SMOD
    | EQ
    | NEQ
    | LT
    | LE
    | SLT
    | SLE ->
      let err =
        Format.sprintf "sel_binop: unsupported operation %s"
          (Bil.string_of_binop o)
      in
      failwith err

  let get_dsts (jmp : jmp term) : (tid * tid) option =
    match Jmp.dst jmp, Jmp.alt jmp with
    | Some dst, Some alt ->
      begin
        match Jmp.resolve dst, Jmp.resolve alt with
        | First dst, First alt -> Some (dst, alt)
        | _ -> None
      end
    | _ -> None


  let sel_unop (o : unop) : arm_pure -> arm_pure =
    match o with
    | NOT -> assert false
    | NEG -> assert false

  let rec select_exp (e : Bil.exp) : arm_pure =
    match e with
    | Load (mem, BinOp (PLUS, a, Int w), _, size) ->
      let mem = select_mem mem in
      let a = select_exp a in
      let w = const w in
      mem @> binop (ldr_op @@ Size.in_bits size) (Imm 32) a w
    | Load (mem, loc, _, size) ->
      let mem = select_exp mem in
      let loc = select_exp loc in
      ldr (Size.in_bits size) mem loc
    | Store (mem, loc, value, _, _size) ->
      let mem = select_exp mem in
      let loc = select_exp loc in
      let value = select_exp value in
      (* We have to swap the arguments here, since ARM likes the value
       first, and the location second *)
      str mem value loc
    (* FIXME: this is amost certainly wrong *)
    | BinOp (PLUS, a, b) when Exp.(a = b) ->
      let a = select_exp a in
      let zero = const (Word.zero 32) in
      let one = const (Word.one 32) in
      shl zero a one
    | BinOp (o, a, b) ->
      let a = select_exp a in
      let b = select_exp b in
      sel_binop o a b
    | UnOp (_, _) -> assert false
    (* Handle memory variables specially? *)
    | Var v -> var v
    | Int w -> const w
    | Cast (_, _, _) -> failwith "select_exp: Cast is unsupported!"
    | Let (_, _, _) -> failwith "select_exp: Let is unsupported!"
    | Unknown (_, _) -> failwith "select_exp: Unknown is unsupported!"
    | Ite (_, _, _) -> failwith "select_exp: Ite is unsupported!"
    | Extract (_, _, _) -> failwith "select_exp: Extract is unsupported!"
    | Concat (_, _) -> failwith "select_exp: Concat is unsupported!"

  and select_mem (m : Bil.exp) : arm_eff =
    let m = select_exp m in
    m.op_eff

  and select_stmt (s : Blk.elt) : arm_eff =
    match s with
    | `Def t ->
      let lhs = Def.lhs t in
      let rhs = Def.rhs t in
      begin
        match Var.typ lhs with
        | Imm _ | Unk ->
          let lhs = Ir.Var (Ir.simple_var lhs) in
          let rhs = select_exp rhs in
          lhs := rhs
        | Mem _ ->
          let rhs = select_exp rhs in
          rhs.op_eff
      end
    | `Jmp jmp ->
      let cond = Jmp.cond jmp in
      begin
        match cond with
        (* FIXME: do some non-trivial pattern matching at this point *)
        | cond ->
          let cond = select_exp cond in
          begin
            match get_dsts jmp with
            | Some (dst, alt) -> br cond dst alt
            | None ->
              let err = Format.asprintf "Unexpected branch: %a" Jmp.pp jmp in
              failwith err
          end
      end
    | `Phi _ -> failwith "select_stmt: Phi nodes are unsupported!"

  and select_elts (elts : Blk.elt list) : arm_eff =
    match elts with
    | [] -> empty_eff
    (* We only select 1 instruction at a time for now *)
    | s :: ss ->
      let s = select_stmt s in
      let ss = select_elts ss in
      s @. ss

  and select_blk (b : blk term) : arm_eff =
    let b_eff = Blk.elts b |> Seq.to_list |> select_elts in
    let {current_data; current_ctrl; other_blks} = b_eff in
    let new_blk =
      Ir.simple_blk (Term.tid b)
        ~data:current_data
        ~ctrl:current_ctrl
    in
    let all_blks = Ir.add new_blk other_blks in
    {
      current_data = [];
      current_ctrl = [];
      other_blks = all_blks
    }

  and select_blks (bs : blk term list) : arm_eff =
    match bs with
    | [] -> empty_eff
    | b :: bs ->
      let b = select_blk b in
      b @. (select_blks bs)

  let select (bs : blk term list) : Ir.t =
    let bs = select_blks bs in
    assert Core_kernel.(List.is_empty bs.current_data
                        && List.is_empty bs.current_ctrl);
    bs.other_blks


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
          | None ->
            let v = Format.asprintf "%a" Theory.Var.pp v in
            Kb_error.fail (Kb_error.Missing_semantics v)
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

  let unk _sort = Kb_error.fail (Kb_error.Not_implemented "Arm_gen.unk")

  let let_ _v _e _b = Kb_error.fail (Kb_error.Not_implemented "Arm_gen.let_")

  let int _sort (w : Theory.word) : 's Theory.bitv =
    (* FIXME: we're assuming every constant is exactly 32
       bits. *)
    let w = Bitvec.to_int32 w in
    pure @@ const @@ Word.of_int32 ~width:32 w

  let add a b =
    let- a = a in
    let- b = b in
    pure @@ a + b

  let mul a b =
    let- a = a in
    let- b = b in
    pure @@ a * b

  let sub a b =
    let- a = a in
    let- b = b in
    pure @@ a - b

  let sdiv a b =
    let- a = a in
    let- b = b in
    pure @@ a / b

  let div a b =
    let- a = a in
    let- b = b in
    pure @@ udiv a b

  let goto (lab : tid) : Theory.ctrl Theory.eff =
    eff @@ b_instr lab

  let jmp addr =
    let- addr_bitv = addr in
    eff @@ jmp addr_bitv

  let repeat _cond _body =
    Kb_error.fail (Kb_error.Not_implemented "Arm_gen.repeat")

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

  let opcode_pretty i : (string, Kb_error.t) result =
    Result.return @@ Ir.Opcode.name i

  (* We use this function when generating ARM, since the assembler
     doesn't like % or @ in labels. *)
  let tid_to_string (t : tid) : string =
    Tid.name t |> String.strip ~drop:Char.(fun c -> c = '%' || c = '@')

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
      | Void -> Result.return ""
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


  (* FIXME: Absolute hack *)
  (* We mark where the bracket location start and end in the argument list. *)
  let mk_loc_list (op : string) (args : 'a list) : bracket list =
    let len = List.length args in
    let init_neither len = List.init len ~f:(fun _ -> Neither) in
    if String.(op = "ldr" || op = "ldrh" || op = "ldrb") then
      begin
        if len = 2 then
          [Neither; Both]
        else if len = 3 then
          [Neither; Open; Close]
        else failwith "mk_loc_list: expected to receive 2 or 3 arguments"
      end
    else
      init_neither len

  let arm_operands_pretty (op : string) (hd : Ir.operand) (l : Ir.operand list)
    : (string, Kb_error.t) result =
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

  let arm_op_pretty (t : Ir.operation) : (string, Kb_error.t) result =
    let op = List.hd_exn t.opcodes in
    Result.(opcode_pretty op >>= fun op ->
            arm_operands_pretty op
              (List.hd_exn t.lhs)
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

let slot = arm_eff

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

(* Returns [None] if the control is fall-through or a conditional jump,
   or if the data section is non-empty. Otherwise, if it is an
   unconditional jump to label [l], returns [Some l]. *)
let is_simple_branch (blk : Ir.blk) : Ir.operand option =
  let open ARM_ops.Ops in
  if not (List.is_empty blk.data) then None
  else
    match blk.ctrl with
    | [{ Ir.opcodes; Ir.operands; _}] ->
      let opcode = List.hd_exn opcodes in
      if Ir.Opcode.(opcode = b) then
        begin
          match operands with
          |[o] -> Some o
          | _ -> None
        end
        else None
    | _ -> None

let is_empty_blk (blk : Ir.blk) : bool =
  (List.is_empty blk.data) && (List.is_empty blk.ctrl)

(* Returns the pair of labels from a [beq l1; b l2] control operation
   in a block, and [None] if not applicable. *)
let get_eq_branches (blk : Ir.blk) : (tid * tid) option =
  let open ARM_ops.Ops in
  match blk.ctrl with
  | [{ Ir.opcodes = ops1; Ir.operands = args1; _};
     { Ir.opcodes = ops2; Ir.operands = args2; _}] ->
    let op1 = List.hd_exn ops1 in
    let op2 = List.hd_exn ops2 in
    if Ir.Opcode.(op1 = beq && op2 = b) then
      begin
        match args1, args2 with
        |[Label l1], [Label l2] -> Some (l1, l2)
        | _ -> None
      end
    else None
  | _ -> None


(*
   Takes a block and a set of blocks, and if the block is of the form:

     beq l1
     b l2

   and the [l1] and [l2] bloks are, respectively:

   l1:
     b l3

   and

   l2: //empty

   then turn the first block into

     beq l3

   and mark the other two blocks for deletion.


*)
(* FIXME: clean this up *)
let opt_jmp_helper (blk : Ir.blk) (blks : Ir.blk list) : (Ir.blk * tid list) option =
  match get_eq_branches blk with
  | None -> None
  | Some (l_true, l_false) ->
    let true_dest =
      List.find blks
        ~f:(fun blk -> Tid.(blk.id = l_true)) |>
      Option.bind ~f:is_simple_branch
    in
    let false_fall =
      List.find blks
        ~f:(fun blk -> Tid.(blk.id = l_false)) |>
      Option.map ~f:is_empty_blk
    in
    begin
      match true_dest, false_fall with
      | Some dest, Some true ->
        let new_blk =
          { blk with
            ctrl =
              [
                Ir.simple_op ARM_ops.Ops.beq Ir.Void [dest]
              ]
          }
        in
        Some (new_blk, [l_true; l_false])
      | _ -> None
    end


let opt_jmp (blks : Ir.blk list) : Ir.blk list =
  let new_blks, to_remove = List.fold blks
      ~init:([], [])
      ~f:(fun (new_blks, to_remove) b ->
          match opt_jmp_helper b blks with
          | None -> (b :: new_blks, to_remove)
          | Some (b_new, new_remove) ->
            (b_new :: new_blks, new_remove @ to_remove))
  in
  List.filter new_blks
    ~f:(fun b -> not @@
         List.mem to_remove b.id ~equal:Tid.equal)


let add_load (op1 : Ir.operation) (op2 : Ir.operation) : Ir.operation option =
  let opc1, opc2 =
    op1.opcodes |> List.hd_exn, op2.opcodes |> List.hd_exn
  in
  if ARM_ops.Ops.(
      (opc1 = add || opc1 = sub)
      &&
      (opc2 = ldr || opc2 = ldrh || opc2 = ldrb))
  then begin
    match op1.lhs, op1.operands, op2.operands with
    (* We are in the case:
       v1 := v2 +/- c; _ := ldr(b|h) x [v3]
    *)
    | [Var v1], [Var v2; Const c], [Var v3] ->
      begin
        let v1 = Option.value_exn v1.pre_assign in
        let v3 = Option.value_exn v3.pre_assign in
        if Var.(v1 = v3) then
          (* Invert c if op1 is a subtraction *)
          let c =
            if ARM_ops.Ops.(opc1 = sub) then
              Word.neg c
            else c
          in
          let res =
            {op2 with
             operands = [Var v2; Const c]
            }
          in
          Some res
        else None
      end
    | _ -> None
  end
  else
    None

let rec opt_add_loads (ops : Ir.operation list) : Ir.operation list =
  match ops with
  | o1 :: o2 :: os ->
    begin
      match add_load o1 o2 with
      | Some o -> o :: (opt_add_loads os)
      | None -> o1 :: (opt_add_loads (o2 :: os))
    end
  | _ -> ops


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
  let opt_add_loads blk =
    let {Ir.data; _} = blk in
    let data = opt_add_loads data in
    { blk with
      data = data
    }
  in
  let ir = Ir.map_blks ir ~f:filter_nops in
  let ir = Ir.map_blks ir ~f:opt_add_loads in
  let ir = { ir with blks = opt_jmp ir.blks } in
  ir


let () =
  Theory.declare
    ~context:["vibes"]
    ~package:"vibes"
    ~name:"arm-gen"
    ~desc:"This theory allows instantiating Program semantics into \
          a Vibes_ir.t term, using Arm_gen.slot."
  @@ KB.return (module ARM_Core : Theory.Core)
