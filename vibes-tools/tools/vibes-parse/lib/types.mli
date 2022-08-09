open Bap_core_theory

(** The parsed C function. *)
type ast = Cabs.definition

module Parsed_c_code : sig

  (** The function call information for a program. *)
  val function_info_slot :
    (Theory.program, Vibes_function_info.Types.t) KB.slot
  
  (** Stash the function call information for a label. *)
  val stash_function_info :
    Theory.label ->
    Vibes_function_info.Types.t ->
    unit KB.t

  (** Retrieve the function call information for a label. *)
  val get_function_info :
    Theory.label ->
    Vibes_function_info.Types.t KB.t

  (** The semantics of the program. *)
  val slot : (Theory.program, Theory.Semantics.t) KB.slot

  (** Provides the semantics of the program. *)
  val set : Theory.label -> Theory.Semantics.t -> unit KB.t
  
end
