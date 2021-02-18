open Core_kernel
open Bap.Std
open Bap_knowledge

module KB = Knowledge

type sol = {
  reg : var Var.Map.t;
  opcode : Ir.opcode Tid.Map.t;
  temp : Var.t Var.Map.t;
  active : bool Tid.Map.t;
  issue : int Tid.Map.t;
} [@@deriving sexp, compare]

module Sol : sig
  module S :
    sig
      type t = sol
      val compare : sol -> sol -> int
      val sexp_of_t : sol -> Ppx_sexp_conv_lib.Sexp.t
    end
  type t = sol
  val sexp_of_t : sol -> Ppx_sexp_conv_lib.Sexp.t
  val ( >= ) : S.t -> S.t -> bool
  val ( <= ) : S.t -> S.t -> bool
  val ( = ) : S.t -> S.t -> bool
  val ( > ) : S.t -> S.t -> bool
  val ( < ) : S.t -> S.t -> bool
  val ( <> ) : S.t -> S.t -> bool
  val equal : S.t -> S.t -> bool
  val compare : S.t -> S.t -> int
  val min : S.t -> S.t -> S.t
  val max : S.t -> S.t -> S.t
  val ascending : S.t -> S.t -> int
  val descending : S.t -> S.t -> int
  val between : S.t -> low:S.t -> high:S.t -> bool
  val clamp_exn : S.t -> min:S.t -> max:S.t -> S.t
  val clamp : S.t -> min:S.t -> max:S.t -> S.t Base__.Or_error.t
  type comparator_witness = Base__Comparable.Make(S).comparator_witness
  val comparator : (S.t, comparator_witness) Base__.Comparator.comparator
  val validate_lbound :
    min:S.t Base__.Maybe_bound.t -> S.t Base__.Validate.check
  val validate_ubound :
    max:S.t Base__.Maybe_bound.t -> S.t Base__.Validate.check
  val validate_bound :
    min:S.t Base__.Maybe_bound.t ->
    max:S.t Base__.Maybe_bound.t -> S.t Base__.Validate.check
end

type sol_set = (sol, Sol.comparator_witness) Core_kernel.Set.t

(**
   [run_minzinc minizinc_model_filepath ir] encodes the provided [ir] IR.t
   to a json file, calls minizinc, and interpets the solution. It uses the
   provided [minizinc_model_filepath] to run minizinc.
*)

val run_minizinc : string -> sol list -> Ir.t -> (Ir.t * sol) KB.t


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
  latency : (opcode , int) mznmap;
  number_excluded : int;
  exclude_reg : (int, (temp, reg) mznmap) mznmap
} [@@deriving yojson]

type serialization_info = {
  temps : Var.t list;
  temp_map : Var.t String.Map.t;
  operations : Tid.t list;
  operands : Var.t list;
}

val serialize_mzn_params : Ir.t -> sol list -> mzn_params_serial * serialization_info



val apply_sol : Ir.t -> sol -> Ir.t
