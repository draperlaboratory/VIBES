open Bap_core_theory

(** Generates assembly for the VIBES IR program according to
    the MiniZinc model. *)
val run :
  target:string ->
  language:string ->
  vir_filepath:string ->
  asm_outfile:string ->
  model_filepath:string ->
  (unit, KB.conflict) result
