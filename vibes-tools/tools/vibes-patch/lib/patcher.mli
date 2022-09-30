open Bap_core_theory

(** A placed patch. *)
type patch = {
  data : string;
  addr : int64;
  loc : int64;
}

type res = patch list * Vibes_patch_info.Types.spaces

(** [patch target language asms ~binary ~patched_binary ~backend ?spaces] will
    assemble and place a list of patches in the original [binary], whose contents
    are written to the file at path [patched_binary].

    The result is the list of patches applied to the binary, as well as the
    resulting list of external spaces that are now occupied.
*)
val patch :
  ?patch_spaces:Vibes_patch_info.Types.spaces ->
  ?backend:string option ->
  Theory.target ->
  Theory.language ->
  Vibes_as.Types.Assembly.t list ->
  binary:string ->
  patched_binary:string ->
  (res, KB.conflict) result
