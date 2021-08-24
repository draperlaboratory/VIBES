open !Core_kernel
open Bap.Std
open Bap_knowledge
open Bap_core_theory

type opcode = Knowledge.Name.t [@@deriving compare, equal, sexp]

module Opcode =
struct

  type t = opcode

  let create ?arch:arch s = Knowledge.Name.create ?package:arch s

  let name o = Knowledge.Name.unqualified o

  let (=) = equal_opcode

end

type op_var = {
  id : Var.t;
  temps : Var.t list;
  pre_assign : var option
} [@@deriving compare, sexp]

let equal_op_var x y = [%equal: Var.t] x.id y.id

let simple_var v =
  {
    id = Var.create ~fresh:true "operand" (Var.typ v);
    temps = [v];
    pre_assign = None
  }

let given_var v ~reg:reg =
  {
   id = Var.create ~fresh:true "operand" (Var.typ v);
    temps = [v];
    pre_assign = Some reg
  }

type operand =
    Var of op_var
  | Const of Word.t
  | Label of Tid.t
  | Void
  | Offset of Word.t [@@deriving compare, equal, sexp]

type shift = [
  | `ASR
  | `LSL
  | `LSR
  | `ROR
  | `RRX
] [@@deriving sexp, equal, compare]


let freshen_operand o =
  match o with
  | Var v ->
    let fresh_v =
      { v with
        id =
          Var.create
            ~is_virtual:true
            ~fresh:true
            (Var.name v.id)
            (Var.typ v.id)
      } in
    Var fresh_v
  | _ -> o

let op_var_exn (x : operand) : op_var =
  match x with
  | Var o -> o
  | _ -> failwith "Expected op_var"

type operation = {
  id : Tid.t;
  lhs : operand list;
  opcodes : opcode list;
  optional : bool;
  operands :  operand list;
} [@@deriving compare, equal, sexp]

let simple_op opcode arg args =
  let tid = Tid.create () in
  { id = tid;
    lhs = [arg];
    opcodes = [opcode];
    optional = false;
    (* Operands need to have unique ids *)
    operands = List.map ~f:freshen_operand args;
  }

let mk_empty_operation () =
  let tid = Tid.create () in
  { id = tid;
    lhs = [];
    opcodes = [];
    optional = false;
    operands = [];
  }

type blk = {
  id : Tid.t;
  data : operation list;
  ctrl : operation list;
  ins : operation;
  outs : operation;
  frequency : int
} [@@deriving compare, equal, sexp]


let simple_blk tid ~data ~ctrl =
  {
    id = tid;
    data= data;
    ctrl = ctrl;
    (* Probably we should just add every variable in ops here *)
    ins = mk_empty_operation ();
    outs = mk_empty_operation ();
    frequency = 1
  }


type t = {
  blks : blk list;
  congruent : (op_var * op_var) list
} [@@deriving compare, equal, sexp]

let empty = {blks = []; congruent = []}

let union t1 t2 =
  let comp_pair = Tuple.T2.compare ~cmp1:compare_op_var ~cmp2:compare_op_var in
  {
    blks =
      List.dedup_and_sort ~compare:compare_blk (t1.blks @ t2.blks);
    congruent =
      List.dedup_and_sort ~compare:comp_pair (t1.congruent @ t2.congruent)
  }

let add blk t =
  {t with blks = blk::t.blks}


let operation_to_string (o : operation) = Tid.to_string o.id
let op_var_to_string (o : op_var) = Var.to_string o.id

let var_operands (ops : operand list) : op_var list =
  List.fold ~f:(fun acc o ->
      match o with
      | Var v -> v :: acc
      | Const _ | Label _ | Void | Offset _ -> acc
    ) ~init:[] ops

module Blk = struct

  let all_operands (blk : blk) : operand list =
    let operation_operands op_list =
      List.concat_map op_list
        ~f:(fun operation ->
            operation.lhs @ operation.operands)
    in
    blk.ins.lhs @
    blk.outs.operands @
    (operation_operands blk.data) @
    (operation_operands blk.ctrl)

  let all_rhs_operands (blk : blk) : operand list =
    let operation_operands op_list =
      List.concat_map op_list
        ~f:(fun operation ->
            operation.operands)
    in
    blk.ins.operands @
    blk.outs.operands @
    (operation_operands blk.data) @
    (operation_operands blk.ctrl)

  let all_lhs_operands (blk : blk) : operand list =
    let operation_operands op_list =
      List.concat_map op_list
        ~f:(fun operation ->
            operation.lhs)
    in
    blk.ins.lhs @ blk.outs.lhs @ (operation_operands blk.data) @ (operation_operands blk.ctrl)

  let all_temps (blk : blk) : Var.Set.t =
    List.concat_map (all_operands blk)
      ~f:(fun op ->
          match op with
          | Const _ | Label _ | Void | Offset _ -> []
          | Var op -> op.temps) |>
    Var.Set.of_list

  let definer_map (blk : blk) : op_var Var.Map.t =
    List.fold
      (all_lhs_operands blk |> var_operands)
      ~init:Var.Map.empty
      ~f:(fun acc operand ->
          List.fold operand.temps
            ~init:acc
            ~f:(fun acc tmp ->
                Var.Map.add_exn acc ~key:tmp ~data:operand))

  let users_map (blk : blk) : (op_var list) Var.Map.t =
    List.fold (all_rhs_operands blk |> var_operands)
      ~init:Var.Map.empty
      ~f:(fun acc operand ->
          List.fold operand.temps ~init:acc ~f:(fun acc temp ->
              Var.Map.update acc temp
                ~f:(fun mrandlist ->
                    match mrandlist with
                    | Some operandlist -> operand :: operandlist
                    | None -> [operand])
            )
        )

  let all_operations (blk : blk) : operation list =
    blk.ins :: ( blk.outs :: (blk.data @ blk.ctrl))

  let operation_opcode (blk : blk) : (opcode list) Tid.Map.t =
    List.fold ~init:Tid.Map.empty ~f:(fun acc o ->
        Tid.Map.add_exn acc ~key:o.id ~data:o.opcodes
      ) (all_operations blk)

  let operand_operation (blk : blk) : operation Var.Map.t =
    let alist =
      List.concat_map (all_operations blk)
        ~f:(fun operation ->
            let rhs_operands =
              List.map (var_operands operation.operands)
                ~f:(fun op -> (op.id, operation))
            in
            let lhs_operands =
              List.map (var_operands operation.lhs)
                ~f:(fun op -> (op.id, operation)) in
            lhs_operands @ rhs_operands)
    in
    Var.Map.of_alist_exn alist

end


let var_map_union_exn m1 m2 =
  Var.Map.merge m1 m2
    ~f:(fun ~key:_ ab ->
        match ab with
        | `Left a -> Some a
        | `Right b -> Some b
        | `Both(_ , _) -> failwith "Map has both keys")

let tid_map_union_exn m1 m2 =
  Tid.Map.merge m1 m2
    ~f:(fun ~key:_ ab ->
        match ab with
        | `Left a -> Some a
        | `Right b -> Some b
        | `Both(_ , _) -> failwith "Map has both keys")

let map_blks ~f (sub : t) : t =
  {blks = List.map ~f sub.blks; congruent = sub.congruent}

let map_operations ~f (vir : t) : t =
  map_blks vir
    ~f:(fun b ->
        { id = b.id;
          data = List.map ~f b.data;
          ctrl = List.map ~f b.ctrl;
          ins = f b.ins;
          outs = f b.outs;
          frequency = b.frequency })

let map_op_vars ~f (vir : t) : t =
  let f2 o = match o with
    | Var o -> Var (f o)
    | o -> o
  in
  let apply_to_op (o : operation) : operation =
    {
      o with
      lhs = List.map ~f:f2 o.lhs;
      operands = List.map ~f:f2 o.operands;
    }
  in
  {
    blks =
      List.map vir.blks
        ~f:(fun b ->
            {
              b with
              data = List.map ~f:apply_to_op b.data;
              ctrl = List.map ~f:apply_to_op b.ctrl;
              ins = apply_to_op b.ins;
              outs = apply_to_op b.outs;
            }
          );
    congruent = List.map ~f:(Tuple2.map ~f:f) vir.congruent;
  }

let all_temps (sub : t) : Var.Set.t =
  List.fold sub.blks
    ~init:Var.Set.empty
    ~f:(fun acc blk ->
        Var.Set.union acc (Blk.all_temps blk))

let all_operands (sub : t) : Var.Set.t =
  List.concat_map sub.blks ~f:Blk.all_operands |>
  var_operands |>
  List.map ~f:(fun (o : op_var) -> o.id) |>
  Var.Set.of_list

let preassign_map (sub : t) : (var option) Var.Map.t =
  List.concat_map sub.blks ~f:Blk.all_operands |>
  var_operands |>
  List.map ~f:(fun op -> (op.id, op.pre_assign))
  |> Var.Map.of_alist_exn


let definer_map (sub : t) : op_var Var.Map.t =
  List.fold sub.blks
    ~init:Var.Map.empty
    ~f:(fun acc blk ->
        var_map_union_exn acc (Blk.definer_map blk))


let users_map (sub : t) : (op_var list) Var.Map.t =
  List.fold sub.blks
    ~init:Var.Map.empty
    ~f:(fun acc blk ->
        let m = Blk.users_map blk in
        Var.Map.merge acc m
          ~f:(fun ~key:_ ab ->
              match ab with
              | `Left a -> Some a
              | `Right b -> Some b
              | `Both(a,b) -> Some (a @ b)))

let temp_blk (sub : t) : Tid.t Var.Map.t =
  List.concat_map sub.blks
    ~f:(fun blk ->
        List.map (Blk.all_temps blk |> Var.Set.to_list)
          ~f:(fun t -> (t, blk.id))) |>
  Var.Map.of_alist_exn

let operation_opcodes (sub : t) : (opcode list) Tid.Map.t =
  List.fold sub.blks
    ~init:Tid.Map.empty
    ~f:(fun acc blk ->
        tid_map_union_exn acc (Blk.operation_opcode blk))

let operand_operation (sub : t) : operation Var.Map.t =
  List.fold sub.blks
    ~init:Var.Map.empty
    ~f:(fun acc blk ->
        var_map_union_exn acc (Blk.operand_operation blk))

let pretty_operand o =
  match o with
  | Var o ->
    sprintf "%s : %s < %s"
      (Var.to_string o.id)
      (List.map ~f:Var.to_string o.temps |> String.concat ~sep:"::")
      ((Option.map
          ~f:(Var.to_string) o.pre_assign) |>
       Option.value ~default:"N/A")
  | Const c -> Word.to_string c
  | Label l -> Tid.to_string l
  | Void -> ""
  | Offset c -> Format.asprintf "Offset(%d)" (Word.to_int_exn c)

let pretty_operand_list l =
  List.map ~f:pretty_operand l |> String.concat ~sep:","

let pretty_operation o =
  sprintf "\t\t[%s]  <- %s [%s]" (pretty_operand_list o.lhs)
    (String.concat
       (List.map o.opcodes ~f:Opcode.name))
    (pretty_operand_list o.operands)

let pretty_blk b = sprintf "blk : %s \n\tins : %s \n\touts: %s\n\tdata: \n%s\n\tctrl: \n%s"
    (Tid.to_string b.id)
    (pretty_operation b.ins)
    (pretty_operation b.outs)
    (List.fold b.data ~init:""
       ~f:(fun acc o -> acc ^ pretty_operation o ^ "\n"))
    (List.fold b.ctrl ~init:""
       ~f:(fun acc o -> acc ^ pretty_operation o ^ "\n"))

let pretty_ir (vir : t) : string =
  List.fold vir.blks ~init:"" ~f:(fun acc b -> acc ^ (pretty_blk b) ^ "\n\n")

let to_string t = pretty_ir t

let dummy_reg_alloc t =
  map_op_vars t
    ~f:(fun v ->
        match v.pre_assign with
        | Some _ -> v
        | None ->
          let var = List.hd_exn v.temps in
          {v with pre_assign = Some (Var.create "R0" (Var.typ var))})

let preassign_var tgt v =
  let regs = Theory.Target.regs tgt |> Set.to_sequence in
  let regs = Seq.map regs ~f:Var.reify in
  if Seq.mem ~equal:Var.equal regs v
  then Some v
  else None

(* We freshen temporaries so that they do not clash with register
   names in the Minizinc model. *)
let freshen_temps v =
  let name = Var.name v in
  let typ = Var.typ v in
  Var.create ("__Vibes_tmp_" ^ name) typ

let preassign (tgt : Theory.target) (ir : t) : t =
  map_op_vars ir
    ~f:(fun v ->
        {v with
         temps = List.map ~f:freshen_temps v.temps;
         pre_assign = List.hd_exn v.temps |> preassign_var tgt
        })
