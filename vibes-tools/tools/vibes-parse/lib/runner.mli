open Bap_core_theory

(** Parses the C program  in [patch_filepath] and compiles to a BIR program,
    which is then serialized to [bir_outfile]. The function call metadata is
    serialized to [func_info_outfile]. *)
val run :
  target:string ->
  patch_info_filepath:string ->
  patch_filepath:string ->
  bir_outfile:string ->
  func_info_outfile:string ->
  (unit, KB.conflict) result
