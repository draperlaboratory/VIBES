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

(** Verifies the patched executable.

    This module is responsible for comparing the patched executable (produced
    by the {!Patcher}) against the original executable, to determine if the
    patched executable is correct.

    This module checks correctness by using CBAT to confirm whether a
    correctness property specified by the user holds of the patched executable
    relative to the original executable. *)

open !Core_kernel
open Bap.Std
open Bap_wp
open Bap_core_theory

(* A [result] record that a [verifier] function can return. *)
type status = Z3.Solver.status

(** A [verifier] function takes a target, two subroutines and a
   correctness property, it verifies their correctness, and it returns
   a {!result}. *)
type verifier =
  Run_parameters.t
  -> Runner.input list
  -> (status, Bap_main.error) result

(** Indicates whether the patching is done, or should be attempted again. *)
type next_step = Done | Again

(** [verify orig_prog patch_prog func property ~verifier ~printer] uses the
    [verifier] to verify that the [func] in the [orig_prog] and [patch_prog]
    satisfies the provided [property]. The [printer] is used to print the
    verifier's results. *)
val verify :
  ?verifier:(verifier) ->
  orig_prog:(Program.t * string * Utils.Code_addrs.t) ->
  patch_prog:(Program.t * string * Utils.Code_addrs.t) ->
  Theory.target ->
  Run_parameters.t ->
  (next_step, Toplevel_error.t) result
