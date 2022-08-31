open Core
open Bap.Std
open Monads.Std
open Bap_core_theory

module Json = Vibes_utils.Json
module Ir = Vibes_ir.Types
module Linear = Vibes_linear_ssa.Utils

module R = Monad.Result.Make(KB.Conflict)(Monad.Ident)

open R.Let

type 'a set = {set : 'a list} [@@deriving yojson]
type ('k, 'v) map = 'v list [@@deriving yojson]
type enum = {e: string} [@@deriving yojson]
type enum_def = enum set [@@deriving yojson]

let set (set : 'a list) : 'a set = {set}
let enum (e : string) : enum = {e}
let enumf (f : 'a -> string) (x : 'a) : enum = {e = f x}

let enum_set : string list -> enum set =
  Fn.compose set @@ List.map ~f:enum

let enum_setf (f : 'a -> string) : 'a list -> enum set =
  Fn.compose set @@ List.map ~f:(enumf f)

type operand = enum [@@deriving yojson]
type operation = enum [@@deriving yojson]
type block = enum [@@deriving yojson]
type temp = enum [@@deriving yojson]
type opcode = enum [@@deriving yojson]
type reg = enum [@@deriving yojson]
type hvar = enum [@@deriving yojson]

type serialization_info = {
  temps : Var.t list;
  temp_map : Var.t String.Map.t;
  reg_map : Var.t String.Map.t;
  operations : Ir.id list;
  operands : Ir.id list;
} [@@deriving equal]

module Solution = struct

  module T = struct

    type t = {
      reg : var Var.Map.t;
      opcode : Ir.opcode Ir.id_map;
      temp : var Ir.id_map;
      active : bool Ir.id_map;
      issue : int Ir.id_map;
    } [@@deriving sexp, compare]

  end

  module C = struct

    include T
    include Base.Comparator.Make(T)

  end

  include C

  type set = (t, comparator_witness) Set.t

  let empty_set : set = Set.empty (module C)

  type serial = {
    reg : (temp, reg) map;
    opcode : (operation, opcode) map;
    temp : (operand, temp) map;
    live : bool list;
    active : bool list;
    issue : int list;
    stary_cycle: int list;
    end_cycle : int list;
    _objective : int;
  } [@@deriving yojson]

  let deserialize
      (filename : string)
      (info : serialization_info) : (t, KB.conflict) result =
    let* serial = Json.from_file filename
        ~yojson_of_t:yojson_of_serial
        ~t_of_yojson:serial_of_yojson in
    try
      let reg =
        let regs =
          List.map serial.reg ~f:(fun r ->
              Map.find_exn info.reg_map r.e) in
        List.zip_exn info.temps regs |>
        Var.Map.of_alist_exn in
      let opcode =
        let opcode = List.map serial.opcode ~f:(fun {e} -> e) in
        List.zip_exn info.operations opcode |>
        Int.Map.of_alist_exn in
      let temp =
        List.map2_exn info.operands serial.temp ~f:(fun op temp ->
            op, Map.find_exn info.temp_map temp.e) |>
        Int.Map.of_alist_exn in
      let active =
        List.zip_exn info.operations serial.active |>
        Int.Map.of_alist_exn in
      let issue =
        List.zip_exn info.operations serial.issue |>
        Int.Map.of_alist_exn in
      Ok {reg; opcode; temp; active; issue}
    with exn ->
      let msg = Format.asprintf
          "Encountered an error when deserializing MiniZinc \
           solution %s: %a" filename Exn.pp exn in
      Error (Errors.Deserialization_failed msg)

  let apply (ir : Ir.t) (solution : t) : Ir.t =
    let ir = Ir.map_blks ir ~f:(fun b ->
        let data = List.filter b.data ~f:(fun o ->
            Map.find solution.active o.id |>
            Option.value ~default:false) in
        {b with data}) in
    let ir = Ir.map_opvars ir ~f:(fun o ->
        Map.find solution.temp o.id |>
        Option.value_map ~default:o ~f:(fun temp ->
            Map.find solution.reg temp |>
            Option.value_map ~default:o ~f:(fun reg ->
                {o with temps = [temp]; preassign = Some reg}))) in
    ir

end

module Params = struct

  type class_t = (operand, (opcode, reg set) map) map [@@deriving yojson]
  type congruent = (operand, operand set) map [@@deriving yojson]
  type exclude_reg = (int, (temp, reg) map) map [@@deriving yojson]
  type hvars_temps = (hvar, temp set) map [@@deriving yojson]

  type t = {
    reg_t : enum_def;
    opcode_t : enum_def;
    temp_t : enum_def;
    hvar_t : enum_def;
    operand_t : enum_def;
    operation_t : enum_def;
    block_t : enum_def;
    class_t : class_t;
    operand_operation : (operand, operation) map;
    definer : (temp, operand) map;
    users : (temp, operand set) map;
    temp_block : (temp, block) map;
    copy : operation set;
    width : (temp, int) map;
    preassign : (operand, reg set) map;
    congruent : congruent;
    operation_opcodes : (operation, opcode set) map;
    latency : (opcode, int) map;
    number_excluded : int;
    exclude_reg : exclude_reg;
    block_outs : (block, operation) map;
    block_ins : (block, operation) map;
    block_operations : (block, operation set) map;
    hvars_temps : hvars_temps;
  } [@@deriving yojson]

  let dummy : var = Var.create "vibes:dummy_reg" Unk

  let regs_of_role
      (r : Theory.role)
      ~(gpr : string list)
      ~(regs : string list) : (reg set, KB.conflict) result =
    if Theory.Role.(r = Register.general) then
      Ok (enum_set gpr)
    else if Theory.Role.(r = Ir.Roles.dummy) then
      Ok (enum_setf Var.to_string [dummy])
    else if Theory.Role.(r = Ir.Roles.preassigned) then
      Ok (enum_set regs)
    else
      let msg = Format.asprintf
          "Unsupported register role: %a"
          Theory.Role.pp r in
      Error (Errors.Unsupported_role msg)

  let width_of_var
      (v : var)
      ~(target : Theory.target) : (int, KB.conflict) result =
    match Var.typ v with
    | Imm n -> Ok (n / Theory.Target.bits target)
    | Mem _ -> Ok 0
    | Unk ->
      let msg = Format.asprintf
          "Width is unimplemented for var %a of type Unk"
          Var.pp v in
      Error (Errors.Invalid_width msg)

  let key_map
      (keys : 'k list)
      (m : ('k, 'v, _) Map.t)
      ~(f : 'a -> 'c) : 'c list =
    List.map keys ~f:(Fn.compose f @@ Map.find_exn m)

  let key_map_d
      (keys : 'k list)
      (m : ('k, 'v, _) Map.t)
      ~(default : 'c)
      ~(f : 'a -> 'c) : 'c list =
    List.map keys ~f:(fun x -> match Map.find m x with
        | None -> default
        | Some x -> f x)

  let serialize_class_t
      (opcodes : Ir.opcode list)
      (operands : Ir.id list)
      (regs : var list)
      (gpr : var list)
      (op_classes : Ir.Roles.map) : (class_t, KB.conflict) result =
    let regs = List.map regs ~f:Var.to_string in
    let gpr = List.map gpr ~f:Var.to_string in
    R.List.map operands ~f:(fun o ->
        match Map.find op_classes o with
        | None -> Ok []
        | Some r ->
          R.List.map opcodes ~f:(fun op ->
              match Map.find r op with
              | None -> Ok (set [])
              | Some r -> regs_of_role r ~gpr ~regs))

  let serialize_congruences
      (ir : Ir.t)
      (temps : var list) : congruent =
    List.map temps ~f:(fun t ->
        match Var.typ t with
        | Type.Mem _ | Type.Unk -> set []
        | Type.Imm _ -> match Map.find ir.congruences t with
          | Some s -> Set.to_list s |> enum_setf Var.to_string
          | None -> set []) 

  let serialize_exclude_reg
      (prev_solutions : Solution.set)
      (temps : var list) : exclude_reg =
    Set.to_list prev_solutions |>
    List.map ~f:(fun (s : Solution.t) ->
        key_map temps s.reg ~f:(enumf Var.to_string))

  let serialize_hvar_t (temp_names : string list) : hvar set =
    List.filter_map temp_names ~f:Linear.orig_name |>
    List.sort ~compare:String.compare |>
    List.map ~f:(fun v -> enum ("hvar_" ^ v)) |>
    set

  let serialize_hvars_temps
      (hvar_t : hvar set)
      (temp_names : string list) : hvars_temps =
    List.map hvar_t.set ~f:(fun {e = hvar} ->
        List.filter temp_names ~f:(fun t ->
            Linear.orig_name t |>
            Option.value_map ~default:false ~f:(String.equal hvar)) |>
        enum_set)

  let regs_gpr (target : Theory.target) : var list * var list =
    let regs =
      Theory.Target.regs target |>
      Set.map (module Var) ~f:Var.reify |>
      Set.to_list in
    let gpr =
      let roles = Theory.Role.Register.[general] in
      let exclude = Theory.Role.Register.[stack_pointer] in
      Theory.Target.regs target ~exclude ~roles |>
      Set.map (module Var) ~f:Var.reify |>
      Set.to_list in
    regs, gpr
  
  let serialize
      ?(prev_solutions : Solution.set = Solution.empty_set)
      (ir : Ir.t)
      (target : Theory.target) : (t * serialization_info, KB.conflict) result =
    let regs, gpr = regs_gpr target in
    let reg_map =
      List.map (dummy :: regs) ~f:(fun r -> Var.to_string r, r) |>
      String.Map.of_alist_exn in
    let temps = Ir.all_temps ir |> Var.Set.to_list in
    let temp_names = List.map temps ~f:Var.to_string in
    let temp_map = List.zip_exn temp_names temps |> String.Map.of_alist_exn in
    let temp_block = Ir.temp_to_block ir in
    let blocks = Map.data temp_block |> List.sort ~compare:Tid.compare in
    let operation_opcodes = Ir.operation_to_opcodes ir in
    let operations = Map.keys operation_opcodes in
    let operands = Ir.all_opvar_ids ir |> Set.to_list in
    let opcodes = Ir.all_opcodes ir in
    let hvar_t = serialize_hvar_t temp_names in
    let operand_operation =
      let f = Fn.compose Int.to_string Ir.Operation.id in
      Ir.operand_to_operation ir |> key_map operands ~f:(enumf f) in
    let definer =
      let f = Fn.compose Int.to_string Ir.Opvar.id in
      Ir.definer_map ir |> key_map temps ~f:(enumf f) in
    let users =
      let f = Fn.compose Int.to_string Ir.Opvar.id in
      Ir.users_map ir |> key_map_d temps ~default:(set []) ~f:(enum_setf f) in
    let* class_t =
      Ir.op_classes ir |>
      serialize_class_t opcodes operands regs gpr in
    let* width = R.List.map temps ~f:(width_of_var ~target) in
    let preassign =
      Ir.opvar_to_preassign ir |>
      key_map_d operands ~default:(set []) ~f:(fun v ->
          enum_setf Var.to_string [v]) in
    let block_outs =
      Ir.block_to_outs ir |> key_map blocks ~f:(enumf Int.to_string) in
    let block_ins =
      Ir.block_to_ins ir |> key_map blocks ~f:(enumf Int.to_string) in
    let block_operations =
      Ir.block_to_operations ir |>
      key_map blocks ~f:(enum_setf Int.to_string) in
    let params = {
      reg_t = Map.keys reg_map |> enum_set;
      opcode_t = enum_set opcodes;
      temp_t = enum_set temp_names;
      hvar_t;
      operand_t = enum_setf Int.to_string operands;
      operation_t = enum_setf Int.to_string operations;
      block_t = enum_setf Tid.to_string blocks;
      class_t;
      operand_operation;
      definer;
      users;
      temp_block = key_map temps temp_block ~f:(enumf Tid.to_string);
      copy = set [];
      width;
      preassign;
      congruent = serialize_congruences ir temps;
      operation_opcodes = key_map operations operation_opcodes ~f:enum_set;
      latency = List.map opcodes ~f:(fun _ -> 1);
      number_excluded = Set.length prev_solutions;
      exclude_reg = serialize_exclude_reg prev_solutions temps;
      block_outs;
      block_ins;
      block_operations;
      hvars_temps = serialize_hvars_temps hvar_t temp_names;
    } in
    Ok (params, {temps; temp_map; reg_map; operations; operands})

end
