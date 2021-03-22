(** Common utilities. *)

open Bap.Std
open Bap_knowledge
module KB = Knowledge

(** [cp src dst] copies the file from the [src] to the [dst] filepath. *)
val cp : string -> string -> unit

(** [run_process command args] runs [command] with [args] *)
val run_process : string -> string list -> (unit, Errors.t) Result.t

(** [lift_kb_result] transforms a computation in the Result.t monad 
    into the KB.t monad. *)
val lift_kb_result : ('a, Errors.t) Result.t -> 'a KB.t

(** [load_exe "/path/to/exe"] loads /path/to/exe into BAP. *)
val load_exe : string -> (project * Program.t, Toplevel_error.t) result

(** [get_sub prog "main"] returns the function ["main"] in [prog]. *)
val get_func : Program.t -> string -> Sub.t
