open Bap_core_theory

module Eval(_ : Theory.Core) : sig

  (** [parse hvars target ast] translates the C function [ast]
      into a Core Theory program. Additionally, it returns function
      call information for the later stages of the compilation
      pipeline. *)
  val parse :
    Vibes_higher_vars.Higher_var.t list ->
    Theory.target ->
    Cabs.definition ->
    (unit Theory.eff * Vibes_function_info.Types.t) KB.t

end
