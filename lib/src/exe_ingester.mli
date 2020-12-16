(* Ingests the original executable.

   This module is responsible for loading the original executable into
   the BAP ecosystem. *)

open Bap.Std
open Bap_knowledge

module KB = Knowledge

(* A [loader] function takes a filepath (a [string]) and lifts that binary
   into a BAP [Project.t]. *)
type loader = string -> Project.t KB.t

(* [ingest ~loader obj] uses the [loader] function to load the original
   executable associated with the provided [obj]. *)
val ingest : ?loader:(loader) -> Data.t -> unit KB.t
