open Bap.Std
open Bap_core_theory

(** [run sub ~target ~language ~patch_info ~func_info ?patch_spaces]
    runs the optimization passes on the subroutine [sub]. *)
val run :
  ?patch_spaces:Vibes_patch_info.Types.spaces ->
  sub term ->
  target:Theory.target ->
  language:Theory.language ->
  patch_info:Vibes_patch_info.Types.t ->
  func_info:Vibes_function_info.Types.t ->
  sub term KB.t
