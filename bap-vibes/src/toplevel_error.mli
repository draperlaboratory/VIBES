(* Custom errors for a top level run of the VIBES pipeline.

   Note that this module only handles errors that occur outside of KB
   computations. The {!Errors} module handles errors that occur inside
   of KB computations. *)

open !Core_kernel

(* Top-level errors that the VIBES pipeline can raise. *)
type t =
  | Failed_to_load_proj of string
  | WP_result_unknown of string
  | Max_tries of int
  | KB_error of Errors.t
  | Other of string

(* [pp ppf e] is a pretty-printer for the error [e] on formatter [ppf]. *)
val pp : Format.formatter -> t -> unit
