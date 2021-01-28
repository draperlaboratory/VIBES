(* Implements {!Vibes_plugin_errors}. *)

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
        "config json field \"func\" must be a non-empty string"
    | Missing_property ->
        "config json field \"property\" must be a non-empty string"
    | Missing_size ->
        "each patch in the config json \"patches\" list must have a "
      ^ "\"patch-size\" field containing an integer"
    | Config_not_parsed s ->
        "error finding or parsing config JSON file: " ^ s
    | Invalid_hex desc -> desc
    | Invalid_property desc -> desc
    | Invalid_patch_code desc -> desc
    | Invalid_max_tries ->
        "optional config json field \"max-tries\" must be an integer"
  in
  Format.fprintf ppf "@[%s@]" msg