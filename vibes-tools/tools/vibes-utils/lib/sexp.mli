open Core
open Bap_core_theory

(** [to_sexp data ~error] attempts to parse [data] as an S-expression.
    Returns [Error] according to [error] if this fails. *)
val to_sexp :
  string ->
  error:(string -> KB.conflict) ->
  (Sexp.t list, KB.conflict) result
