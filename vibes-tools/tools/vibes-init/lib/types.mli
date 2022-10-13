open Bap_core_theory

(** Information for generating a patch. *)
type patch = {
  name : string;
  patch : string;
  patch_info : string;
  func_info : string;
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
}

(** Creates the information for generating the patch build process. *)
val create :
  Theory.target ->
  Theory.language ->
  patch_names:string list ->
  model:string ->
  binary:string ->
  patched_binary:string ->
  spaces:string ->
  t

(** Pretty-prints the Makefile for running the pipeline. *)
val pp_makefile : Format.formatter -> t -> unit

(** Generates empty files for starting a new patch project. *)
val generate_files : t -> (unit, KB.conflict) result
