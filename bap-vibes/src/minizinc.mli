(***************************************************************************)
(*                                                                         *)
(*  Copyright (C) 2022/2023 The Charles Stark Draper Laboratory, Inc.      *)
(*                                                                         *)
(*  This file is provided under the license found in the LICENSE file in   *)
(*  the top-level directory of this project.                               *)
(*                                                                         *)
(*  This research was developed with funding from the Defense Advanced     *)
(*  Research Projects Agency (DARPA).                                      *)
(*                                                                         *)
(***************************************************************************)

open Core_kernel
open Bap.Std
open Bap_knowledge
open Bap_core_theory
open Minizinc_utils
module KB = Knowledge


(** [sol] is a solution returned by minizinc.
    It is unlikely that you need introspect inside this data type outside the minizinc
    module.

    As of the functionality of vibes 2/25/21 the only two fields that are of interest are
       [reg] and [issue]

    Its fields include:
    [reg] is a mapping from temporaries to registers. It holds the solution to the
      register allocation problem.
    [opcode] is a mapping from operations to opcodes. It is interesting in the case of
      there being multiple possible opcodes for an operation.
    [temp] is a mapping from operands to temps. It is interesing in the case where
      there are multiple logical temporaries available due to copying.
    [active] is a mapping from operations to booleans. It is interesting when there
      are optional instructions for copying. If it is false, the operation should be
      deleted from the Ir.
    [issue] is a mapping from operations to the issue cycle on which they execute.
      The ordering of these numbers is the ordering of the assembly to emit.

*)

type sol = {
  reg : var Var.Map.t;
  opcode : Ir.opcode Int.Map.t;
  temp : Var.t Var.Map.t;
  active : bool Int.Map.t;
  issue : int Int.Map.t;
} [@@deriving sexp, compare]

(**

   [run_allocation_and_scheduling tgt lang minizinc_model_filepath ir] encodes the
   provided [ir] IR.t for target [tgt] in language [lang] to a json
   file, calls minizinc, and interpets the solution. It uses the
   provided [minizinc_model_filepath] to run minizinc.  It will
   exclude the solutions in the [sol list] parameter.

*)

val run_allocation_and_scheduling :
  ?congruence:(var * var) list ->
  ?exclude_regs:String.Set.t ->
  Theory.target ->
  sol list ->
  Ir.t ->
  filepath:string ->
  gpr:Var.Set.t ->
  regs:Var.Set.t -> (Ir.t * sol) KB.t

(** This is a module necessary for building Sets of [sol] *)
module Sol : sig
  module S :
  sig
    type t = sol
    val compare : sol -> sol -> int
    val sexp_of_t : sol -> Ppx_sexp_conv_lib.Sexp.t
  end
  type t = sol
  val sexp_of_t : sol -> Ppx_sexp_conv_lib.Sexp.t
  val compare : S.t -> S.t -> int
  type comparator_witness = Base__Comparable.Make(S).comparator_witness
  val comparator : (S.t, comparator_witness) Base__.Comparator.comparator
end

type sol_set = (sol, Sol.comparator_witness) Core_kernel.Set.t

(* Exposed for unit testing. *)

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
  class_t : (operand, (opcode, reg mzn_set) mzn_map) mzn_map;
  operand_operation : (operand, operation) mzn_map;
  definer : (temp, operand) mzn_map;
  users : (temp, operand mzn_set) mzn_map;
  temp_block : (temp, block) mzn_map;
  copy : operation mzn_set;
  width : (temp, int) mzn_map;
  preassign : (operand, reg mzn_set) mzn_map; (* Set should either be empty or have 1 element. *)
  congruent : (operand, operand mzn_set) mzn_map;
  operation_opcodes : (operation, opcode mzn_set) mzn_map;
  latency : (opcode, int) mzn_map;
  number_excluded : int;
  exclude_reg : (int, (temp, reg) mzn_map) mzn_map;
  block_outs : (block, operation) mzn_map;
  block_ins : (block, operation) mzn_map
} [@@deriving yojson]

type serialization_info = {
  temps : Var.t list;
  temp_map : Var.t String.Map.t;
  operations : Int.t list;
  operands : Var.t list;
} [@@deriving equal]

val serialize_mzn_params :
  ?congruence:(var * var) list ->
  ?exclude_regs:String.Set.t ->
  Theory.target ->
  Ir.t ->
  sol list ->
  gpr:Var.Set.t ->
  regs:Var.Set.t -> (mzn_params_serial * serialization_info) KB.t

val apply_sol : Ir.t -> sol -> Ir.t
