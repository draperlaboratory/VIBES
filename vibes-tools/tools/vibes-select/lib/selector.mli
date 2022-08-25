open Bap.Std
open Bap_core_theory

(** Runs the instruction selector, returning the VIBES IR program. *)
val run :
  sub term ->
  hvars:Vibes_higher_vars.Higher_var.t list ->
  target:Theory.target ->
  language:Theory.language ->
  Vibes_ir.Types.t KB.t
