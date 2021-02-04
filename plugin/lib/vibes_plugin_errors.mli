(* Errors for the plugin. 

   Mostly these are errors that arise when parsing the CLI arguments and
   configuration file provided by the user. *)

type t =
  | Missing_exe
  | Missing_patches
  | Missing_patch_name
  | Missing_patch_code
  | Missing_patch_point
  | Missing_func
  | Missing_property
  | Missing_size
  | Missing_minizinc_model_filepath
  | Config_not_parsed of string
  | Invalid_hex of string
  | Invalid_property of string
  | Invalid_patch_code of string
  | Invalid_max_tries
  | No_such_file of string

val pp : Format.formatter -> t -> unit
