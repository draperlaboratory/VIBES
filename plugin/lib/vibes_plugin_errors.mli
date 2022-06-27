(***************************************************************************)
(*                                                                         *)
(*  Copyright (C) 2022/2023 The Charles Stark Draper Laboratory, Inc.      *)
(*                                                                         *)
(*  This file is provided under the license found in the LICENSE file in   *)
(*  the top-level directory of this project.                               *)
(*                                                                         *)
(*  This research was developed with funding from the Defense Advanced     *)
(*  Research Projects Agency (DARPA).                                      *)
(*                                                                         *)
(***************************************************************************)

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
  | Invalid_perform_verification
  | Invalid_loader_data of string
  | Invalid_bsi_data of string
  | Invalid_init_mem
  | Loader_data_conflict
  | Invalid_minizinc_isel_filepath
  | Invalid_extra_constraints
  | No_such_file of string
  | Bad_image of string * Core_kernel.Error.t

val pp : Format.formatter -> t -> unit
