open Bap_core_theory

(** Attempts to patch the binary with the provided assembly code. *)
val run :
  target:string ->
  language:string ->
  patch_info_filepath:string ->
  binary:string ->
  asm_filepath:string ->
  patched_binary:string ->
  (unit, KB.conflict) result
