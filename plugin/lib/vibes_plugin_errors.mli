(** Errors for the plugin.

   Mostly these are errors that arise when parsing the CLI arguments and
   configuration file provided by the user. *)

type t =
  | Missing_exe
  | Missing_patches
  | Missing_patch_name
  | Missing_patch_code
  | Missing_patch_point
  | Missing_func
  | Missing_wp_params
  | Missing_size
  | Missing_minizinc_model_filepath
  | Missing_higher_var_name
  | Missing_higher_var_stored_in
  | Missing_higher_var_reg
  | Missing_higher_var_fp
  | Missing_higher_var_offset
  | Missing_higher_var_at_entry
  | Missing_higher_var_at_exit
  | Config_not_parsed of string
  | Invalid_sp_align of string
  | Invalid_hex of string
  | Invalid_property of string
  | Invalid_patch_code of string
  | Invalid_patch_spaces of string
  | Invalid_max_tries
  | Invalid_loader_data of string
<<<<<<< HEAD
  | Invalid_bsi_data of string
  | Loader_data_conflict
=======
  | Invalid_minizinc_isel_filepath
>>>>>>> 79663b8... reworked isel path to use option
  | No_such_file of string
  | Bad_image of string * Core_kernel.Error.t

val pp : Format.formatter -> t -> unit
