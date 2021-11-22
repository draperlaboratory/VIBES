(* Implements {!Vibes_plugin_errors}. *)

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
  | Invalid_hex of string
  | Invalid_property of string
  | Invalid_patch_code of string
  | Invalid_max_tries
  | Invalid_loader_data of string
  | Invalid_bsi_data of string
  | No_such_file of string
  | Bad_image of string * Core_kernel.Error.t

let pp (ppf : Format.formatter) t : unit =
  let msg = match t with
    | Missing_exe -> "missing the filepath of an executable"
    | Missing_patches ->
      "config json field \"patches\" is mandatory and must be a json array"
    | Missing_patch_name ->
      "each patch in the config json \"patches\" list must have a "
      ^ "\"patch-name\" field containing a non-empty string"
    | Missing_patch_code ->
      "each patch in the config json \"patches\" list must have a "
      ^ "\"patch-code\" field containing a non-empty string"
    | Missing_patch_point ->
      "each patch in the config json \"patches\" list must have a "
      ^ "\"patch-point\" field containing a non-empty string"
    | Missing_func ->
      "config json field \"wp-params\" requires a non-empty \"func\" field"
    | Missing_wp_params ->
      "config json field \"wp-params\" is mandatory and must be a json struct"
    | Missing_size ->
      "each patch in the config json \"patches\" list must have a "
      ^ "\"patch-size\" field containing an integer"
    | Missing_minizinc_model_filepath ->
      "missing the filepath for a minizinc model"
    | Missing_higher_var_name ->
      "missing 'name' field for a higher variable"
    | Missing_higher_var_stored_in ->
      "missing 'stored-in' field for a higher variable"
    | Missing_higher_var_reg ->
      "missing 'register' field for a higher variable"
    | Missing_higher_var_fp ->
      "missing 'frame-pointer' field for a higher variable"
    | Missing_higher_var_offset ->
      "missing 'offset' field for a higher variable"
    | Missing_higher_var_at_entry ->
      "missing 'at-entry' field for a higher variable"
    | Missing_higher_var_at_exit ->
      "missing 'at-exit' field for a higher variable"
    | Config_not_parsed s ->
      "error finding or parsing config JSON file: " ^ s
    | Invalid_hex desc -> desc
    | Invalid_property desc -> desc
    | Invalid_patch_code desc -> desc
    | Invalid_max_tries ->
      "optional config json field \"max-tries\" must be an integer"
    | Invalid_loader_data s ->
      "error in optional loader data field \"ogre\": " ^ s
    | Invalid_bsi_data s ->
      "error in optional field \"bsi-metadata\": " ^ s
    | No_such_file desc -> desc
    | Bad_image (exe, e) ->
      Format.asprintf "couldn't create image for exe \"%s\": %a"
        exe Core_kernel.Error.pp e

  in
  Format.fprintf ppf "@[%s@]" msg
