open Bap_core_theory

(** A placed patch. *)
type patch = {
  data : string;
  addr : int64;
  loc : int64;
  len : int64;
}

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
  (patch, KB.conflict) result  
