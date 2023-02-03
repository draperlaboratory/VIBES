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

(** Runs the selector and serializes the results to [vir_outfile]. *)
val run :
  target:string ->
  language:string ->
  patch_info_filepath:string ->
  bir_filepath:string ->
  vir_outfile:string ->
  (unit, KB.conflict) result
