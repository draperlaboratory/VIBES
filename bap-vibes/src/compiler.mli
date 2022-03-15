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

(** Compiles the patch.

    This module is responsible for taking the patch code (BIL statements)
    that was ingested by the {!Patch_ingester}, and "compiling" it to
    assembly (or something like it) for the target architecture. *)

open Core_kernel
open Bap.Std
open Bap_core_theory

(** [compile_ir obj] converts the patch (which is BIL) associated with the
    provided [obj] into VIBES IR. It stores this IR
    into slots of the patches of the [obj]. *)
val compile_ir : ?isel_model_filepath:string option -> Data.t -> unit KB.t

type solver =
  ?congruence:(var * var) list ->
  ?exclude_regs:String.Set.t ->
  ?extra_constraints:string option ->
  Theory.target ->
  Minizinc.sol list ->
  Ir.t ->
  filepath:string ->
  gpr:Var.Set.t ->
  regs:Var.Set.t ->
  (Ir.t * Minizinc.sol) KB.t

(** [compile_assembly obj] converts the patch IR associated with the
    provided [obj] into assembly. It stores this assembly and the minizinc
    solution into slots of the patches of the [obj]. [compile_assembly] also takes
    an optional [solver] parameter for unit testing which defaults
    to [Minizinc.run_minizinc] *)
val compile_assembly :
  ?solver:solver ->
  Data.t ->
  unit KB.t
