open Bap.Std
open Bap_core_theory

(** Transforms the subroutine into linear SSA form. *)
val transform :
  Vibes_higher_vars_lib.Higher_var.t list ->
  sub term ->
  Types.t KB.t
