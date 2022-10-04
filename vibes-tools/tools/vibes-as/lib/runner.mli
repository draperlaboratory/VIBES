open Bap_core_theory

(** Generates assembly for the VIBES IR program according to
    the MiniZinc model. *)
val run :
  ?extra_constraints_filepath:string option ->
  target:string ->
  language:string ->
  vir_filepath:string ->
  patch_info_filepath:string ->
  asm_outfile:string ->
  model_filepath:string ->
  unit ->
  (unit, KB.conflict) result
