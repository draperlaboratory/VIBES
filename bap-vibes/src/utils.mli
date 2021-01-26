(* Common utilities. *)
open Bap_knowledge
module KB = Knowledge

(* [cp src dst] copies the file from the [src] to the [dst] filepath. *)
val cp : string -> string -> unit

(* [run_process_exn command args] run command with args *)
val run_process_exn : string -> string list -> unit KB.t