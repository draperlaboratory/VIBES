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

(** Information for generating a patch. *)
type patch = {
  name : string;
  patch : string;
  patch_info : string;
  bir : string;
  bir_opt : string;
  vir : string;
  asm : string;
  constraints : string;
}

(** Information about the patch build process. *)
type t = {
  target : Theory.target;
  language : Theory.language;
  model : string;
  binary : string;
  patched_binary : string;
  patches : patch list;
  spaces : string;
  ogre : string option;
}

(** Creates the information for generating the patch build process. *)
val create :
  ?ogre:string option ->
  ?language:Theory.language option ->
  patch_names:string list ->
  model:string ->
  binary:string ->
  patched_binary:string ->
  spaces:string ->
  unit -> (t, KB.conflict) result

(** Pretty-prints the Makefile for running the pipeline. *)
val pp_makefile : Format.formatter -> t -> unit

(** Generates empty files for starting a new patch project. *)
val generate_files : t -> (unit, KB.conflict) result
