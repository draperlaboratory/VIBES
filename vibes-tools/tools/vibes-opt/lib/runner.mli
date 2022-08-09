open Bap_core_theory

(** Runs the optimizer and serializes the results to [bir_outfile]. *)
val run :
  target:string ->
  language:string ->
  patch_info_filepath:string ->
  bir_filepath:string ->
  func_info_filepath:string ->
  bir_outfile:string ->
  (unit, KB.conflict) result
