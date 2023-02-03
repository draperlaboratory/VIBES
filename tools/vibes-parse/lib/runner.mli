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

(** Parses the C program  in [patch_filepath] and compiles to a BIR program,
    which is then serialized to [bir_outfile]. *)
val run :
  target:string ->
  patch_info_filepath:string ->
  patch_filepath:string ->
  bir_outfile:string ->
  (unit, KB.conflict) result
