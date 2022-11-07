open Bap.Std
open Bap_core_theory

val call_with_args : (Theory.program, Var.Set.t) KB.slot

module Make(_ : Theory.Core) : sig

  (** [compile hvars target ast] translates the C function [ast]
      into a Core Theory program. Additionally, it returns function
      call information for the later stages of the compilation
      pipeline. *)
  val compile :
    Vibes_higher_vars.Higher_var.t list ->
    Theory.target ->
    Cabs.definition ->
    Theory.Semantics.t KB.t

end
