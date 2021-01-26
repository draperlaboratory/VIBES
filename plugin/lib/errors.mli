(* Errors for the plugin. 

   Mostly these are configuration errors. *)

type t =
  | Missing_exe
  | Missing_patches
  | Missing_patch_name
  | Missing_patch_code
  | Missing_patch_point
  | Missing_func
  | Missing_property
  | Missing_size
  | Config_not_parsed of string
  | Invalid_hex of string
  | Invalid_property of string
  | Invalid_patch_code of string
  | Invalid_max_tries

val pp : Format.formatter -> t -> unit
