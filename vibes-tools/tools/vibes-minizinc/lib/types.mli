open Core
open Bap.Std
open Bap_core_theory

module Ir = Vibes_ir.Types

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

module Solution : sig

  type t = {
    reg : var Var.Map.t;
    opcode : Ir.opcode Ir.id_map;
    temp : var Ir.id_map;
    active : bool Ir.id_map;
    issue : int Ir.id_map;
  } [@@deriving sexp, compare]

  include Comparator.S with type t := t

  type set = (t, comparator_witness) Set.t

  val empty_set : set

  val deserialize :
    string ->
    serialization_info ->
    (t, KB.conflict) result

  val apply : Ir.t -> t -> Ir.t

end

module Params : sig

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

  val serialize :
    ?prev_solutions:Solution.set ->
    Ir.t ->
    target:Theory.target ->
    gpr:Var.Set.t ->
    regs:Var.Set.t ->
    (t * serialization_info, KB.conflict) result

end
