(** Ingests the patch code.

    This module is responsible for taking the patch code provided by the
    user, and loading that (possibly lifting it) to BIR. *)

open Bap_knowledge
module KB = Knowledge

(** [ingest ()] registers the callback which loads the patch code
   (i.e., lifs it to BIR) which is associated with the provided
   toplevel [Data.cls]. *)
val ingest : unit -> unit
