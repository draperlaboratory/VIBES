open Bap_core_theory

(** Attempts to patch the binary with the provided assembly code. *)
val run :
  ?patch_spaces:string option ->
  ?ogre_filepath:string option ->
  target:string ->
  language:string ->
  binary:string ->
  asm_filepaths:string list ->
  patched_binary:string ->
  unit ->
  (unit, KB.conflict) result
