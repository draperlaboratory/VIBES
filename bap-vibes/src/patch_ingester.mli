(** Ingests the patch code.

    This module is responsible for taking the patch code provided by the
    user, and loading that (possibly lifting it) to BIR. *)

open Bap_knowledge
module KB = Knowledge

val ingest : Data.t -> unit KB.t
(** [ingest obj] loads the patch code (i.e., lifs it to BIR) which is
    associated with the provided [obj]. *)
