(* Common utilities. *)
open Bap_knowledge
module KB = Knowledge

(* [cp src dst] copies the file from the [src] to the [dst] filepath. *)
val cp : string -> string -> unit

(* [run_process command args] run command with args *)
val run_process : string -> string list -> (unit, Errors.t) Result.t

val lift_kb_result : ('a, Errors.t) Result.t -> 'a KB.t