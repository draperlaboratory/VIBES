(** Gathers useful info about an executable. *)

open Bap.Std
open Bap_knowledge

module KB = Knowledge

(** [extract obj proj] processes the provided [proj] and stores the relevant
    info about with the provided [obj]. *)
val extract : Data.t -> Project.t -> unit KB.t
