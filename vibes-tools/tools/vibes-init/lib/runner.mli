open Bap_core_theory

(** Runs the VIBES initializer; generates a Makefile for the project as
    well as empty/template files. *)
val run :
  target:string ->
  language:string ->
  patch_names:string list ->
  model_filepath:string ->
  binary:string ->
  patched_binary:string ->
  unit ->
  (unit, KB.conflict) result
