open Bap_core_theory
open Vibes_higher_vars

(** [to_core ast target hvars] compiles the parsed C program [ast] into
    a Core Theory program and promises the semantics, which should then
    be available to any of the currently instantiated theories (such as
    BIL, BIR, etc). Also returns the function call metadata. *)
val to_core :
  Types.ast ->
  Theory.target ->
  Higher_var.t list ->
  (Theory.label * Vibes_function_info.Types.t) KB.t
