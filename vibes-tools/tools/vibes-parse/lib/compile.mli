open Bap_core_theory
open Vibes_higher_vars

(** [to_core ast target hvars] provides the semantics of the parsed C
    program to all currently available theories. *)
val to_core :
  Types.ast ->
  Theory.target ->
  Higher_var.t list ->
  Theory.label KB.t
