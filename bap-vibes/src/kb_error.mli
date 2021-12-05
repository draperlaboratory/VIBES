(** Errors that can occur during a [KB.run] in the VIBES pipeline.

    See the {!Toplevel_error} module for errors that can occur in the VIBES
    pipeline outside of a [KB.run]. *)

open Bap_knowledge
module KB = Knowledge

(** Errors that the VIBES pipeline can raise while inside a [KB.run]. *)
type t =
  | Failed_to_load_proj of string
  | Not_implemented of string
  | Missing_minizinc_model_filepath
  | Missing_patch_name
  | Missing_patch_code
  | Missing_assembly
  | Missing_original_exe_filepath
  | Missing_target
  | Missing_patched_exe_filepath
  | Missing_tmp_patched_exe_filepath
  | Missing_patch_point
  | Missing_patch_size
  | Missing_lower_patch_code
  | Missing_patch_vars
  | Missing_patch_space_offset
  | Missing_patch_space_size
  | Missing_func
  | Missing_property
  | Missing_raw_ir
  | Missing_semantics of string
  | Command_not_found of string
  | Patch_code_not_parsed of string
  | Incorrect_patch_point of string
  | Higher_vars_not_substituted of string
  | Exit_code of string
  | Unexpected_exit of string
  | WP_result_unknown of string
  | Max_tries of int
  | Minizinc_deserialization of string
  | Core_c_error of string
  | Other of string

(** [pp ppf e] is a pretty-printer for the error [e] on formatter [ppf]. *)
val pp : Format.formatter -> t -> unit

(** These errors are subsumed under KB conflicts. *)
type KB.Conflict.t += Problem of t

(** Triggers a failure in the VIBES pipeline. *)
val fail : t -> 'a KB.t
