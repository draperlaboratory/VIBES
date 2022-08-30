open Bap_core_theory

(** Runs the selector and serializes the results to [vir_outfile]. *)
val run :
  target:string ->
  language:string ->
  patch_info_filepath:string ->
  bir_filepath:string ->
  vir_outfile:string ->
  (unit, KB.conflict) result
