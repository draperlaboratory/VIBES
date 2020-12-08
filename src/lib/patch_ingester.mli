(* Ingests the patch code.

   This module is responsible for taking the patch code provided by the
   user, and loading that (possibly lifting it) to BIL. *)

open Bap_knowledge
module KB = Knowledge

(* [ingest obj] loads the patch code (i.e., lifs it to BIL) which is
   associated with the provided [obj]. *)
val ingest : Data.t -> unit KB.t
