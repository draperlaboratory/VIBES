(* Some utilities for use in the plugin. *)

(* [realpath path] returns the Unix realpath for [path], or an error. *)
val realpath : string -> (string, Errors.t) result
