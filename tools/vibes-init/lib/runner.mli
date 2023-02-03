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

open Bap_core_theory

(** Runs the VIBES initializer; generates a Makefile for the project as
    well as empty/template files. *)
val run :
  ?ogre:string option ->
  ?language:string option ->
  patch_names:string list ->
  model_filepath:string ->
  binary:string ->
  patched_binary:string ->
  unit ->
  (unit, KB.conflict) result
