open Core
open Bap.Std
open Bap_core_theory

module Utils = Vibes_utils.Misc

type opcode = string [@@deriving compare, equal]

type 'a opcode_map = 'a String.Map.t

type id = int [@@deriving compare, equal]

type 'a id_map = 'a Int.Map.t

type id_set = Int.Set.t

module Roles = struct

  type map = Theory.role opcode_map id_map

  let package = Vibes_constants.Bap_kb.package

  let dummy = Theory.Role.declare ~package "dummy"
  let preassigned = Theory.Role.declare ~package "preassigned"

  let map_of_role
      (opcodes : opcode list)
      (key : id)
      (role : Theory.role) : map =
    let data =
      List.map opcodes ~f:(fun o -> o, role) |>
      String.Map.of_alist_exn in
    Int.Map.singleton key data

end

let fresh_id =
  let next = ref 0 in
  fun () ->
    let id = !next in
    incr next;
    id

module Opvar = struct

  type t = {
    id : id;
    temps : var list;
    preassign : var option;
  } [@@deriving compare]

  let create ?(preassign : var option = None) (v : var) : t = {
    id = fresh_id ();
    temps = [v];
    preassign;
  }

  let equal x y = equal_id x.id y.id

  let pp_temps (ppf : Format.formatter) (temps : var list) : unit =
    let rec aux ppf = function
      | [] -> ()
      | [x] -> Format.fprintf ppf "%a" Var.pp x
      | x :: rest ->
        Format.fprintf ppf "%a :: %a" Var.pp x aux rest in
    aux ppf temps

  let pp_preassign (ppf : Format.formatter) (preassign : var option) : unit =
    match preassign with
    | Some p -> Format.fprintf ppf "%a" Var.pp p
    | None -> Format.fprintf ppf "N/A"

  let pp (ppf : Format.formatter) (t : t) : unit =
    Format.fprintf ppf "%d : %a < %a"
      t.id pp_temps t.temps pp_preassign t.preassign

end

module Operand = struct

  type t =
    | Var of Opvar.t
    | Const of Word.t
    | Label of Tid.t
    | Void of Opvar.t
    | Offset of Word.t
  [@@deriving compare, equal]

  let freshen : t -> t = function
    | Var v -> Var {v with id = fresh_id ()}
    | Void v -> Void {v with id = fresh_id ()}
    | o -> o

  let var_operands : t -> Opvar.t list = function
    | Var v | Void v -> [v]
    | Const _ | Label _ | Offset _ -> []

  let op_classes (opcodes : opcode list) (t : t) : Roles.map = match t with
    | Const _ | Label _ | Offset _ -> Int.Map.empty
    | Void v -> Roles.map_of_role opcodes v.id Roles.dummy
    | Var v ->
      let role = match v.preassign with
        | None -> Theory.Role.Register.general
        | Some _ -> Roles.preassigned in
      Roles.map_of_role opcodes v.id role

  let pp (ppf : Format.formatter) : t -> unit = function
    | Var v -> Format.fprintf ppf "%a" Opvar.pp v
    | Void v -> Format.fprintf ppf "(void) %a" Opvar.pp v
    | Const c -> Format.fprintf ppf "%a" Word.pp c
    | Label l -> Format.fprintf ppf "%a" Tid.pp l
    | Offset o -> Format.fprintf ppf "Offset(%a)" Word.pp o

end

module Operation = struct

  type t = {
    id : id;
    lhs : Operand.t list;
    opcodes : opcode list;
    optional : bool;
    operands : Operand.t list;
  } [@@deriving compare, equal]

  let create_empty () : t = {
    id = fresh_id ();
    lhs = [];
    opcodes = [];
    optional = false;
    operands = [];
  }

  let create_simple
      ?(optional : bool = false) 
      (opcode : opcode)
      (lhs : Operand.t)
      (operands : Operand.t list) : t = {
    id = fresh_id ();
    lhs = [lhs];
    opcodes = [opcode];
    optional;
    operands = List.map operands ~f:Operand.freshen;
  }

  let create_multi_lhs
      ?(optional : bool = false) 
      (opcode : opcode)
      (lhs : Operand.t list)
      (operands : Operand.t list) : t = {
    id = fresh_id ();
    lhs;
    opcodes = [opcode];
    optional;
    operands = List.map operands ~f:Operand.freshen;
  }

  let create_void
      ?(optional : bool = false)
      (opcode : opcode) : t = {
    id = fresh_id ();
    lhs = [];
    opcodes = [opcode];
    optional;
    operands = [];
  }

  let all_operands (t : t) : Operand.t list = t.lhs @ t.operands
  let lhs_operands (t : t) : Operand.t list = t.lhs
  let rhs_operands (t : t) : Operand.t list = t.operands

  let freshen (t : t) : t = {
    t with id = fresh_id ();
  }

  let op_classes (opcodes : opcode list) (t : t) : Roles.map =
    all_operands t |> List.fold ~init:Int.Map.empty ~f:(fun acc op ->
        Operand.op_classes opcodes op |>
        Map.merge acc ~f:(fun ~key -> function
            | `Left a | `Right a -> Some a
            | `Both _ ->
              failwithf "op_classes: duplicate key %d" key ()))

  let pp_operands (ppf : Format.formatter) (ops : Operand.t list) : unit =
    let rec aux ppf = function
      | [] -> ()
      | [x] -> Format.fprintf ppf "%a" Operand.pp x
      | x :: rest -> Format.fprintf ppf "%a, %a" Operand.pp x aux rest in
    aux ppf ops

  let pp_opcodes (ppf : Format.formatter) (ops : opcode list) : unit =
    let rec aux ppf = function
      | [] -> ()
      | [x] -> Format.fprintf ppf "%s" x
      | x :: rest -> Format.fprintf ppf "%s/%a" x aux rest in
    aux ppf ops

  let pp (ppf : Format.formatter) (t : t) : unit =
    Format.fprintf ppf "%d: [%a] <- %a [%a]"
      t.id pp_operands t.lhs pp_opcodes t.opcodes pp_operands t.operands

end

module Block = struct

  type t = {
    tid : Tid.t;
    data : Operation.t list;
    ctrl : Operation.t list;
    ins : Operation.t;
    outs : Operation.t;
    frequency : int;
  } [@@deriving compare, equal]

  let create_simple
      ?(frequency : int = 1)
      (tid : tid)
      ~(data : Operation.t list)
      ~(ctrl : Operation.t list) : t = {
    tid;
    data;
    ctrl;
    ins = Operation.create_empty ();
    outs = Operation.create_empty ();
    frequency;
  }

  let all_operands (t : t) : Operand.t list =
    t.ins.lhs @
    t.outs.operands @
    List.concat_map t.data ~f:Operation.all_operands @
    List.concat_map t.ctrl ~f:Operation.all_operands

  let all_lhs_operands (t : t) : Operand.t list =
    t.ins.lhs @
    t.outs.lhs @
    List.concat_map t.data ~f:Operation.lhs_operands @
    List.concat_map t.ctrl ~f:Operation.lhs_operands

  let all_rhs_operands (t : t) : Operand.t list =
    t.ins.operands @
    t.outs.operands @
    List.concat_map t.data ~f:Operation.rhs_operands @
    List.concat_map t.ctrl ~f:Operation.rhs_operands

  let all_temps (t : t) : Var.Set.t =
    let open Operand in
    all_operands t |> List.concat_map ~f:(function
        | Var v | Void v -> v.temps
        | Const _ | Label _ | Offset _ -> []) |>
    Var.Set.of_list

  let definer_map (t : t) : Opvar.t Var.Map.t =
    all_lhs_operands t |>
    List.concat_map ~f:Operand.var_operands |>
    List.fold ~init:Var.Map.empty ~f:(fun init o ->
        List.fold o.Opvar.temps ~init ~f:(fun acc key ->
            Map.add_exn acc ~key ~data:o))

  let users_map (t : t) : Opvar.t list Var.Map.t =
    all_rhs_operands t |>
    List.concat_map ~f:Operand.var_operands |>
    List.fold ~init:Var.Map.empty ~f:(fun init o ->
        List.fold o.Opvar.temps ~init ~f:(fun acc key ->
            Map.add_multi acc ~key ~data:o))

  let all_operations (t : t) : Operation.t list =
    t.ins :: t.outs :: (t.data @ t.ctrl)

  let operation_to_opcodes (t : t) : opcode list id_map =
    all_operations t |>
    List.fold ~init:Int.Map.empty ~f:(fun acc (o : Operation.t) ->
        Map.add_exn acc ~key:o.id ~data:o.opcodes)

  let operand_to_operation (t : t) : Operation.t id_map =
    all_operations t |> List.concat_map ~f:(fun (o : Operation.t) ->
        let lhs =
          List.concat_map o.lhs ~f:Operand.var_operands |>
          List.map ~f:(fun op -> op.Opvar.id, o) in
        let rhs =
          List.concat_map o.operands ~f:Operand.var_operands |>
          List.map ~f:(fun op -> op.Opvar.id, o) in
        lhs @ rhs) |>
    Int.Map.of_alist_exn

  let op_classes (opcodes : opcode list) (t : t) : Roles.map =
    all_operations t |> List.fold ~init:Int.Map.empty ~f:(fun acc o ->
        Operation.op_classes opcodes o |>
        Map.merge acc ~f:(fun ~key -> function
            | `Left a | `Right a -> Some a
            | `Both _ ->
              failwithf "op_classes: duplicate key %d" key ()))

  let pp_operations (ppf : Format.formatter) (ops : Operation.t list) : unit =
    let rec aux ppf = function
      | [] -> ()
      | [x] -> Format.fprintf ppf "\t%a\n" Operation.pp x
      | x :: rest -> Format.fprintf ppf "\t%a\n\t%a" Operation.pp x aux rest in
    aux ppf ops

  let pp (ppf : Format.formatter) (t : t) : unit =
    Format.fprintf ppf
      "blk : %a\n\
       \tins : %d: [%a]\n\
       \touts : %d: [%a]\n\n\
       \tdata : %a\n\
       \tctrl : %a"
      Tid.pp t.tid
      t.ins.id  Operation.pp_operands t.ins.lhs
      t.outs.id Operation.pp_operands t.outs.operands
      pp_operations t.data
      pp_operations t.ctrl

end

type t = {
  blks : Block.t list;
  congruences : Var.Set.t Var.Map.t;
} [@@deriving compare, equal]

let empty : t = {
  blks = [];
  congruences = Var.Map.empty;
}

let union (x : t) (y : t) : t =
  let blks =
    Utils.dedup_list_stable (x.blks @ y.blks)
      ~compare:Block.compare in
  let congruences =
    Map.merge_skewed x.congruences y.congruences
      ~combine:(fun ~key:_ -> Var.Set.union) in
  {blks; congruences}

let add (blk : Block.t) (t : t) : t = {
  t with blks = blk :: t.blks;
}

let map_blks (t : t) ~(f : Block.t -> Block.t) : t = {
  t with blks = List.map t.blks ~f;
}

let map_operations (t : t) ~(f : Operation.t -> Operation.t) : t =
  map_blks t ~f:(fun blk -> Block.{
      blk with data = List.map blk.data ~f;
               ctrl = List.map blk.ctrl ~f;
               ins = f blk.ins;
               outs = f blk.outs;
    })

let map_operands (t : t) ~(f : Operand.t -> Operand.t) : t =
  map_operations t ~f:(fun o ->  Operation.{
      o with lhs = List.map o.lhs ~f;
             operands = List.map o.operands ~f;
    })

let map_opvars (t : t) ~(f : Opvar.t -> Opvar.t) : t =
  let operand op = Operand.(match op with
      | Var v -> Var (f v)
      | Void v -> Void (f v)
      | Const _ | Label _ | Offset _ -> op )in
  let operation o = Operation.{
      o with lhs = List.map o.lhs ~f:operand;
             operands = List.map o.operands ~f:operand;
    } in
  map_blks t ~f:(fun blk -> Block.{
      blk with data = List.map blk.data ~f:operation;
               ctrl = List.map blk.ctrl ~f:operation;
               ins = operation blk.ins;
               outs = operation blk.outs;
    })

let freshen_operands (t : t) : t = map_operands t ~f:Operand.freshen
let freshen_operation_ids (t : t) : t = map_operations t ~f:Operation.freshen

let all_temps (t : t) : Var.Set.t =
  List.fold t.blks ~init:Var.Set.empty ~f:(fun acc blk ->
      Var.Set.union acc @@ Block.all_temps blk)

let all_opvar_ids (t : t) : id_set =
  List.concat_map t.blks ~f:Block.all_operands |>
  List.concat_map ~f:Operand.var_operands |>
  List.map ~f:(fun v -> v.Opvar.id) |>
  Int.Set.of_list

let opvar_to_preassign (t : t) : var id_map =
  List.concat_map t.blks ~f:Block.all_operands |>
  List.concat_map ~f:Operand.var_operands |>
  List.filter_map ~f:(fun v ->
      Option.map v.Opvar.preassign
        ~f:(fun p -> v.Opvar.id, p)) |>
  Int.Map.of_alist_exn

let definer_map (t : t) : Opvar.t Var.Map.t =
  List.fold t.blks ~init:Var.Map.empty ~f:(fun acc blk ->
      Block.definer_map blk |> Map.merge acc ~f:(fun ~key -> function
          | `Left a | `Right a -> Some a
          | `Both _ ->
            failwithf "definer_map: duplicate key %s"
              (Var.to_string key) ()))

let users_map (t : t) : Opvar.t list Var.Map.t =
  List.fold t.blks ~init:Var.Map.empty ~f:(fun acc blk ->
      Block.users_map blk |> Map.merge acc ~f:(fun ~key:_ -> function
          | `Left a | `Right a -> Some a
          | `Both (a, b) ->
            Some (Utils.dedup_list_stable (a @ b)
                    ~compare:Opvar.compare)))

let temp_to_block (t : t) : tid Var.Map.t =
  List.concat_map t.blks ~f:(fun blk ->
      Block.all_temps blk |> Var.Set.to_list |>
      List.map ~f:(fun t -> t, blk.tid)) |>
  Var.Map.of_alist_exn

let operation_to_opcodes (t : t) : opcode list id_map =
  List.fold t.blks ~init:Int.Map.empty ~f:(fun acc blk ->
      Block.operation_to_opcodes blk |>
      Map.merge acc ~f:(fun ~key -> function
          | `Left a | `Right a -> Some a
          | `Both _ ->
            failwithf "operation_to_opcodes: duplicate key %d" key ()))

let operand_to_operation (t : t) : Operation.t id_map =
  List.fold t.blks ~init:Int.Map.empty ~f:(fun acc blk ->
      Block.operand_to_operation blk |>
      Map.merge acc ~f:(fun ~key -> function
          | `Left a | `Right a -> Some a
          | `Both _ ->
            failwithf "operand_to_operation: duplicate key %d" key ()))

let all_opcodes (t : t) : opcode list =
  List.fold t.blks ~init:String.Set.empty ~f:(fun init blk ->
      Block.all_operations blk |> List.fold ~init ~f:(fun init o ->
          List.fold o.Operation.opcodes ~init ~f:Set.add)) |>
  Set.to_list

let op_classes (t : t) : Roles.map =
  let opcodes = all_opcodes t in
  List.fold t.blks ~init:Int.Map.empty ~f:(fun acc blk ->
      Block.op_classes opcodes blk |> Map.merge acc ~f:(fun ~key -> function
          | `Left a | `Right a -> Some a
          | `Both _ ->
            failwithf "op_classes: duplicate key %d" key ()))

let block_to_ins (t : t) : id Tid.Map.t =
  Tid.Map.of_alist_exn @@
  List.map t.blks ~f:(fun blk ->
      blk.tid, blk.ins.id)

let block_to_outs (t : t) : id Tid.Map.t =
  Tid.Map.of_alist_exn @@
  List.map t.blks ~f:(fun blk ->
      blk.tid, blk.ins.id)

let block_to_operations (t : t) : id list Tid.Map.t =
  Tid.Map.of_alist_exn @@
  List.map t.blks ~f:(fun blk ->
      let ops =
        Block.all_operations blk |>
        List.map ~f:(fun o -> o.Operation.id) in
      blk.tid, ops)

let populate_ins_outs
    (t : t)
    (ins_outs : (Var.Set.t * Var.Set.t) Tid.Map.t) : t =
  let operands vars = Var.Set.to_list vars |> List.map ~f:(fun v->
      let typ = Var.typ v in
      let v = Opvar.create v in
      match typ with
      | Imm _ -> Operand.Var v
      | Mem _ -> Operand.Void v
      | Unk   -> Operand.Void v) in
  map_blks t ~f:(fun blk ->
      match Tid.Map.find ins_outs blk.tid with
      | None -> blk
      | Some (ins, outs) ->
        let in_ops = operands ins in
        let out_ops = operands outs in
        let ins = {blk.ins with lhs = blk.ins.lhs @ in_ops} in
        let outs = {blk.outs with operands = blk.outs.operands @ out_ops} in
        {blk with ins; outs})

let pp_blks (ppf : Format.formatter) (blks : Block.t list) : unit =
  let rec aux ppf = function
    | [] -> ()
    | [x] -> Format.fprintf ppf "%a" Block.pp x
    | x :: rest -> Format.fprintf ppf "%a\n\n%a" Block.pp x aux rest in
  aux ppf blks

let pp (ppf : Format.formatter) (t : t) : unit =
  Format.fprintf ppf "%a" pp_blks t.blks
