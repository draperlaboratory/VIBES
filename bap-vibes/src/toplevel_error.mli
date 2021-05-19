(** Custom errors for a top level run of the VIBES pipeline.

    Note that this module only handles errors that occur outside of KB
    computations. The {!Kb_errors} module handles errors that occur inside
    of KB computations. *)

open! Core_kernel

(** Top-level errors that the VIBES pipeline can raise. *)
type t =
  | Failed_to_load_proj of string
  | WP_failure of Bap_main.error
  | WP_result_unknown of string
  | Max_tries of int
  | No_value_in_KB of string
  | Missing_func_orig of string
  | Missing_func_patched of string
  | KB_error of Kb_error.t
  | Other of string

val pp : Format.formatter -> t -> unit
(** [pp ppf e] is a pretty-printer for the error [e] on formatter [ppf]. *)
