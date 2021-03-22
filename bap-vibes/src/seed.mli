(** Extracts info from [KB.run] results, to seed new {!Data.t}s. *)

open Bap_knowledge
module KB = Knowledge

(** This type represents seed info about a patch. *)
type patch

(** This type represents a bundle of seed info. *)
type t

(** [patches t] returns the list of [patch] seed info in [t]. *)
val patches : t -> patch list

(** [patch_name patch] returns the [patch_name] associated with
    the provided [patch] seed info. *)
val patch_name : patch -> string

(** [minizinc_solutions patch] returns the Minizinc solution set associated
    with the provided [patch] seed info. *)
val minizinc_solutions : patch -> Minizinc.sol_set

(** [patch_with_name t "foo"] finds in [t] the seed info for the
    patch named [foo]. *) 
val patch_with_name : t option -> string -> patch option

(** [extract r s] takes a {!Data.computed} result [r] and the state [s] of
    the KB and it extracts seed info [t] from that result. The resulting seed
    [t] can be used to seed a new [Data.t] via {!create}. *)
val extract : Data.computed -> KB.state -> (t, Toplevel_error.t) result

(** [create config ~seed] takes the provided {!Config.t}, and it creates a
    new {!Data.t} KB object. If any [seed] info is provided, the new KB
    object will be seeded with that info. *)
val create : ?seed:(t option) -> Config.t -> Data.t KB.t
