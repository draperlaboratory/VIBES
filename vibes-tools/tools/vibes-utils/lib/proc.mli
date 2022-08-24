open Bap_core_theory

(** Data observed on stdout. *)
type stdout_data = string list

(** Data observed on stderr. *)
type stderr_data = string list

(** The result of running the command. *)
type cmd_result = (stdout_data * stderr_data, KB.conflict) result

(** [run command args] spawns a new process with command [command] and
    arguments [args], returning the output of the process if successful. *)
val run : string -> string list -> cmd_result
