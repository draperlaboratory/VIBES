open Bap_core_theory

(** Runs the parser and serializes the BIR program to [bir_outfile],
    as well as the function call information to [func_info_outfile]. *)
val run :
  target:string ->
  patch_info_filepath:string ->
  patch_filepath:string ->
  bir_outfile:string ->
  func_info_outfile:string ->
  (unit, KB.conflict) result
