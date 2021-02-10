open Core_kernel
open Bap.Std
open Bap_knowledge

module KB = Knowledge

(**
   [run_minzinc minizinc_model_filepath ir] encodes the provided [ir] IR.t
   to a json file, calls minizinc, and interpets the solution. It uses the
   provided [minizinc_model_filepath] to run minizinc.
*)

val run_minizinc : string -> Ir.t -> Ir.t KB.t


(**/**)
(* Exposed for unit testing. *)

type 'a mznset = {set : 'a list}  [@@deriving yojson]
type ('a ,'b) mznmap = 'b list

type mzn_enum = {e : string} [@@deriving yojson]
type mzn_enum_def = mzn_enum mznset [@@deriving yojson] (* https://github.com/MiniZinc/libminizinc/issues/441 *)
type operand = mzn_enum [@@deriving yojson]
type operation = mzn_enum [@@deriving yojson]
type block = mzn_enum [@@deriving yojson]
type temp = mzn_enum [@@deriving yojson]
type opcode = mzn_enum [@@deriving yojson]
type reg = mzn_enum [@@deriving yojson]

type mzn_params_serial = {
  reg_t : mzn_enum_def;
  opcode_t : mzn_enum_def;
  temp_t : mzn_enum_def;
  operand_t : mzn_enum_def;
  operation_t : mzn_enum_def;
  block_t : mzn_enum_def;
  class_t : mzn_enum_def;
  operand_operation : (operand, operation) mznmap;
  definer : (temp, operand) mznmap;
  users : (temp, operand mznset) mznmap;
  temp_block : (temp, block) mznmap;
  copy : operation mznset;
  width : (temp, int) mznmap;
  preassign : (operand, reg mznset) mznmap; (* Set should either be empty or have 1 element. *)
  congruent : (operand, operand mznset) mznmap;
  operation_opcodes : (operation, opcode mznset) mznmap;
  latency : (opcode , int) mznmap
} [@@deriving yojson]

type serialization_info = {
  temps : Var.t list;
  temp_map : Var.t String.Map.t;
  operations : Tid.t list;
  operands : Var.t list;
}

val serialize_mzn_params : Ir.t -> mzn_params_serial * serialization_info

type sol = {
  reg : var Var.Map.t;
  opcode : Ir.opcode Tid.Map.t;
  temp : Var.t Var.Map.t;
  active : bool Tid.Map.t;
  issue : int Tid.Map.t;
}

val apply_sol : Ir.t -> sol -> Ir.t
