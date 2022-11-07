open Bap_core_theory

(** Runs the optimizer and serializes the results to [bir_outfile]. *)
val run :
  ?patch_spaces:string option ->
  target:string ->
  language:string ->
  patch_info_filepath:string ->
  bir_filepath:string ->
  bir_outfile:string ->
  unit ->
  (unit, KB.conflict) result
