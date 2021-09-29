(** Utilities for initializing/seeding the KB for a pipeline run. *)

open Bap.Std
open Bap_knowledge
module KB = Knowledge

(** This type represents a bundle of info we can seed the KB with. *)
type t

(** [extract_seed r s] takes a {!Data.computed} result [r] and the state [s]
    of the KB and it extracts seed info [t] from that result. The resulting
    seed [t] can be used to seed a new [Data.t] via {!init_KB}. *)
val extract_seed : Data.computed -> KB.state -> t

(** [init_KB config ~seed] initializes the KB for a new pipeline run.
    The KB {Data.t} object it returns is built from the provided [config],
    and any extra [seed] info. *)
val init_KB : ?seed:(t option) -> Config.t -> Project.t -> Data.t KB.t
