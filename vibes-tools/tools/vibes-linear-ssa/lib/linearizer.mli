open Bap.Std
open Bap_core_theory
open Vibes_higher_vars

(** Transforms the subroutine into linear SSA form. *)
val transform :
  sub term ->
  hvars:Higher_var.t list ->
  sub term KB.t
