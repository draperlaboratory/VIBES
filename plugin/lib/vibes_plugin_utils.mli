(* Some utilities for use in the plugin. *)

(* [shell cmd] runs [cmd] in the shell, returns the output (a string list). *)
val shell : string -> string list

(* [realpath path] returns the realpath for [path]. *)
val realpath : string -> string
