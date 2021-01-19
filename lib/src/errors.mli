(* Custom errors for the VIBES pipeline. *)

open Bap_knowledge
module KB = Knowledge

(* Errors that the VIBES pipeline can raise. *)
type t =
  | Failed_to_load_proj of string
  | Not_implemented of string
  | Missing_patch_name
  | Missing_patch_code
  | Missing_assembly
  | Missing_original_exe_filepath
  | Missing_original_exe_prog
  | Missing_addr_size
  | Missing_patched_exe_filepath
  | Missing_tmp_patched_exe_filepath
  | Missing_patch_point
  | Missing_patch_size
  | Missing_func
  | Missing_property
  | Missing_semantics of string
  | Command_not_found of string
  | Patch_code_not_parsed of string
  | Exit_code of string
  | Unexpected_exit of string
  | WP_result_unknown of string
  | Max_tries of int
  | Minizinc_deserialization of string
  | Other of string

(* [pp ppf e] is a pretty-printer for the error [e] on formatter [ppf]. *)
val pp : Format.formatter -> t -> unit

(* These errors are subsumed under KB conflicts. *)
type KB.Conflict.t += Problem of t

(* Triggers a failure in the VIBES pipeline. *)
val fail : t -> 'a KB.t
