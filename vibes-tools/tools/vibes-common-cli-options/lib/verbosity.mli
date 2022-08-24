(** Sets up the verbosity of the logging output.

    - [verbose] enables verbose logging.
    - [no_color] disables TTY color output (only plain ASCII
      will appear in the output).
*)
val setup : verbose:bool -> no_color:bool -> unit

(** Enables verbose logging. *)
val verbose : bool Cmdliner.Term.t

(** Disables TTY color output. *)
val no_color : bool Cmdliner.Term.t
