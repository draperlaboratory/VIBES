open Bap_core_theory

module Make(_ : Theory.Core) : sig

  (** [compile hvars target ast] translates the C function [ast]
      into a Core Theory program. Additionally, it returns function
      call information for the later stages of the compilation
      pipeline. *)
  val compile :
    Vibes_higher_vars.Higher_var.t list ->
    Theory.target ->
    Cabs.definition ->
    (Theory.Semantics.t * Vibes_function_info.Types.t) KB.t

end
