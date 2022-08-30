open Core
open Bap.Std
open Monads.Std
open Bap_core_theory

module Json = Vibes_utils.Json
module Ir = Vibes_ir.Types
module Linear = Vibes_linear_ssa.Utils

module R = Monad.Result.Make(KB.Conflict)(Monad.Ident)

open R.Let

type 'a set = 'a list [@@deriving yojson]
type ('k, 'v) map = 'v list [@@deriving yojson]
type enum = string [@@deriving yojson]
type enum_def = enum set [@@deriving yojson]
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
        List.zip_exn info.temps
          (List.map serial.reg ~f:(Map.find_exn info.reg_map)) |>
        Var.Map.of_alist_exn in
      let opcode =
        List.zip_exn info.operations serial.opcode |>
        Int.Map.of_alist_exn in
      let temp =
        List.map2_exn info.operands serial.temp ~f:(fun op temp ->
            op, Map.find_exn info.temp_map temp) |>
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

  type t = {
    reg_t : enum_def;
    opcode_t : enum_def;
    temp_t : enum_def;
    hvar_t : enum_def;
    operand_t : enum_def;
    operation_t : enum_def;
    block_t : enum_def;
    class_t : (operand, (opcode, reg set) map) map;
    operand_operation : (operand, operation) map;
    definer : (temp, operand) map;
    users : (temp, operand set) map;
    temp_block : (temp, block) map;
    copy : operation set;
    width : (temp, int) map;
    preassign : (operand, reg set) map;
    congruent : (operand, operand set) map;
    operation_opcodes : (operation, opcode set) map;
    latency : (opcode, int) map;
    number_excluded : int;
    exclude_reg : (int, (temp, reg) map) map;
    block_outs : (block, operation) map;
    block_ins : (block, operation) map;
    block_operations : (block, operation set) map;
    hvars_temps : (hvar, temp set) map;
  } [@@deriving yojson]

  let dummy : var = Var.create "vibes:dummy_reg" Unk

  let regs_of_role
      (r : Theory.role)
      ~(gpr : string list)
      ~(regs : string list) : (string list, KB.conflict) result =
    if Theory.Role.(r = Register.general) then Ok gpr
    else if Theory.Role.(r = Ir.Roles.dummy) then Ok [Var.to_string dummy]
    else if Theory.Role.(r = Ir.Roles.preassigned) then Ok regs
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

  let serialize
      ?(prev_solutions : Solution.set = Solution.empty_set)
      (ir : Ir.t)
      ~(target : Theory.target)
      ~(gpr : Var.Set.t)
      ~(regs : Var.Set.t) : (t * serialization_info, KB.conflict) result =
    let reg_map =
      Set.to_list regs |> List.cons dummy |>
      List.map ~f:(fun r -> Var.to_string r, r) |>
      String.Map.of_alist_exn in
    let temps = Ir.all_temps ir |> Var.Set.to_list in
    let temp_names = List.map temps ~f:Var.to_string in
    let temp_map = List.zip_exn temp_names temps |> String.Map.of_alist_exn in
    let temp_block = Ir.temp_to_block ir in
    let blocks = Map.data temp_block |> List.sort ~compare:Tid.compare in
    let operation_opcodes = Ir.operation_to_opcodes ir in
    let operations = Map.keys operation_opcodes in
    let operands = Ir.all_opvar_ids ir |> Set.to_list in
    let reg_t = Map.keys reg_map in
    let opcode_t = Ir.all_opcodes ir in
    let temp_t = temp_names in
    let hvar_t =
      List.filter_map temp_names ~f:Linear.orig_name |>
      List.sort ~compare:String.compare |>
      List.map ~f:(fun v -> "hvar_" ^ v) in
    let operand_t = List.map operands ~f:Int.to_string in
    let operation_t = List.map operations ~f:Int.to_string in
    let block_t = List.map blocks ~f:Tid.to_string in
    let operand_operation =
      let m = Ir.operand_to_operation ir in
      List.map operands ~f:(fun o ->
          let op = Map.find_exn m o in
          Int.to_string op.id) in
    let definer =
      let m = Ir.definer_map ir in
      List.map temps ~f:(fun t ->
          let d = Map.find_exn m t in
          Int.to_string d.id) in
    let op_classes = Ir.op_classes ir in
    let opcodes = Ir.all_opcodes ir in
    let* class_t =
      let gpr = Var.Set.to_list gpr |> List.map ~f:Var.to_string in
      let regs = Var.Set.to_list regs |> List.map ~f:Var.to_string in
      R.List.map operands ~f:(fun o ->
          match Map.find op_classes o with
          | None -> Ok []
          | Some r ->
            R.List.map opcodes ~f:(fun op ->
                match Map.find r op with
                | None -> Ok []
                | Some r -> regs_of_role r ~gpr ~regs)) in
    let users =
      let m = Ir.users_map ir in
      List.map temps ~f:(fun t ->
          match Map.find m t with
          | None -> []
          | Some ops ->
            List.map ops ~f:(fun o -> Int.to_string o.id)) in
    let temp_block = List.map temps ~f:(fun t ->
        let b = Map.find_exn temp_block t in
        Tid.to_string b) in
    let copy = [] in
    let* width = R.List.map temps ~f:(width_of_var ~target) in
    let preassign =
      let m = Ir.opvar_to_preassign ir in
      List.map operands ~f:(fun o ->
          match Map.find m o with
          | Some r -> [Var.to_string r]
          | None -> []) in
    let congruent =
      List.map temps ~f:(fun t ->
          match Var.typ t with
          | Type.Mem _ | Type.Unk -> []
          | Type.Imm _ -> match Map.find ir.congruences t with
            | Some s -> Set.to_list s |> List.map ~f:Var.to_string
            | None -> []) in
    let operation_opcodes = List.map operations ~f:(fun o ->
        Map.find_exn operation_opcodes o) in
    let latency = List.map opcodes ~f:(fun _ -> 1) in
    let number_excluded = Set.length prev_solutions in
    let exclude_reg =
      Set.to_list prev_solutions |>
      List.map ~f:(fun (s : Solution.t) ->
          List.map temps ~f:(fun t ->
              Var.to_string @@ Map.find_exn s.reg t)) in
    let block_ins =
      let m = Ir.block_to_ins ir in
      List.map blocks ~f:(fun b ->
          Int.to_string @@ Map.find_exn m b) in
    let block_outs =
      let m = Ir.block_to_outs ir in
      List.map blocks ~f:(fun b ->
          Int.to_string @@ Map.find_exn m b) in
    let block_operations =
      let m = Ir.block_to_operations ir in
      List.map blocks ~f:(fun b ->
          Map.find_exn m b |> List.map ~f:Int.to_string) in
    let hvars_temps =
      List.map hvar_t ~f:(fun hvar ->
          List.filter temp_names ~f:(fun t ->
              match Linear.orig_name t with
              | Some name -> String.(name = hvar)
              | None -> false)) in
    let params = {
      reg_t;
      opcode_t;
      temp_t;
      hvar_t;
      operand_t;
      operation_t;
      block_t;
      class_t;
      operand_operation;
      definer;
      users;
      temp_block;
      copy;
      width;
      preassign;
      congruent;
      operation_opcodes;
      latency;
      number_excluded;
      exclude_reg;
      block_outs;
      block_ins;
      block_operations;
      hvars_temps;
    } in
    Ok (params, {temps; temp_map; reg_map; operations; operands})

end
