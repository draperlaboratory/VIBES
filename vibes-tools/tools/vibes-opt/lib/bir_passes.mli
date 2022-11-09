open Bap.Std
open Bap_core_theory

(** [run sub ~target ~language ~patch_info ?patch_spaces]
    runs the optimization passes on the subroutine [sub]. *)
val run :
  ?patch_spaces:Vibes_patch_info.Types.Spaces.t ->
  sub term ->
  target:Theory.target ->
  language:Theory.language ->
  patch_info:Vibes_patch_info.Types.t ->
  sub term KB.t
