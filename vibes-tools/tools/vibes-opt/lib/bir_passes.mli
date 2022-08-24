open Bap.Std
open Bap_core_theory

(** [run sub ~target ~language ~patch_info ~func_info] runs
    the optimization passes on the subroutine [sub]. *)
val run :
  sub term ->
  target:Theory.target ->
  language:Theory.language ->
  patch_info:Vibes_patch_info.Types.t ->
  func_info:Vibes_function_info.Types.t ->
  sub term KB.t
