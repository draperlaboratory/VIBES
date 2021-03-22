(** Ingests the original executable.

    This module is responsible for gathering information about the
    original executable. *)

open Bap.Std
open Bap_knowledge

module KB = Knowledge

(** [ingest obj proj] processes the provided [proj] and stores the relevant
    info about with the provided [obj]. *)
val ingest : Data.t -> Project.t -> unit KB.t
