(** Ingests the patch code.

    This module is responsible for taking the patch code provided by the
    user, and loading that (possibly lifting it) to BIR. *)

open Bap_knowledge
module KB = Knowledge

(** Processes the whole patch associated with the [Data.t] argument,
    populating all the relevant KB slots with semantic data associated
    with the patch syntax. *)
val ingest : Data.t -> unit KB.t
