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

(** Attempts to patch the binary with the provided assembly code. *)
val run :
  ?ogre:string option ->
  ?patch_spaces:string option ->
  target:string ->
  language:string ->
  binary:string ->
  asm_filepaths:string list ->
  patched_binary:string ->
  unit ->
  (unit, KB.conflict) result
