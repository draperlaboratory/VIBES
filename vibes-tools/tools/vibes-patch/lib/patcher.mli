open Bap_core_theory

(** [patch patch_info target language asm ~binary ~patched_binary ~backend]
    will assemble and place a patch in the original [binary], whose contents
    are written to the file at path [patched_binary]. *)
val patch :
  ?backend:string option ->
  Vibes_patch_info.Types.t ->
  Theory.target ->
  Theory.language ->
  Vibes_as.Types.Assembly.t ->
  binary:string ->
  patched_binary:string ->
  (unit, KB.conflict) result  
