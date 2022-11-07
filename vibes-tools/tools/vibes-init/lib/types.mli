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
  ?language:Theory.language option ->
  patch_names:string list ->
  model:string ->
  binary:string ->
  patched_binary:string ->
  spaces:string ->
  (t, KB.conflict) result

(** Pretty-prints the Makefile for running the pipeline. *)
val pp_makefile : Format.formatter -> t -> unit

(** Generates empty files for starting a new patch project. *)
val generate_files : t -> (unit, KB.conflict) result
