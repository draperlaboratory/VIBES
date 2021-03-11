(* Ingests the original executable.

   This module is responsible for loading the original executable into
   the BAP ecosystem. *)

open Bap.Std
open Bap_knowledge

module KB = Knowledge

(* A [loader] function takes a filepath (a [string]) and lifts that binary
   into a BAP [Project.t]. *)
type loader = string -> Project.t KB.t

(* [ingest obj proj] processes the provided [proj] and stores the relevant
   info about with the provided [obj]. *)
val ingest : Data.t -> Project.t -> unit KB.t
