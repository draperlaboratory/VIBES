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

open Bap.Std
open Bap_core_theory

(** Runs the instruction selector, returning the VIBES IR program. *)
val run :
  sub term ->
  hvars:Vibes_higher_vars.Higher_var.t list ->
  target:Theory.target ->
  language:Theory.language ->
  Vibes_ir.Types.t KB.t
