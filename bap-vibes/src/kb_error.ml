(* Implements {!Kb_error}. *)

open Bap_knowledge

module KB = Knowledge

(* Errors we want to raise explicitly during a [KB.run]. *)
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

(* A pretty-printer for these errors. *)
let pp ppf (e : t) =
  let msg = match e with
    | Failed_to_load_proj s -> s
    | Not_implemented s -> s
    | Missing_minizinc_model_filepath ->
      "No filepath for a minizinc model was stashed in KB"
    | Missing_patch_name -> "No patch name was stashed in KB"
    | Missing_patch_code -> "No patch code was stashed in KB"
    | Missing_assembly -> "No patch assembly was stashed in KB"
    | Missing_original_exe_filepath ->
      "No filepath for the original exe was stashed in KB"
    | Missing_target -> "No target was stashed in KB"
    | Missing_patched_exe_filepath ->
      "No filepath for the patched exe was stashed in KB"
    | Missing_tmp_patched_exe_filepath ->
      "No filepath for the temporary patched exe was stashed in KB"
    | Missing_patch_point -> "No patch point was stashed in KB"
    | Missing_patch_size -> "No patch size was stashed in KB"
    | Missing_lower_patch_code -> "No lower patch code was stashed in KB"
    | Missing_patch_vars -> "No patch vars were stashed in KB"
    | Missing_patch_space_offset -> "No patch space offset was stashed in KB"
    | Missing_patch_space_size -> "No patch space size was stashed in KB"
    | Missing_func -> "No function name to verify was stashed in KB"
    | Missing_property -> "No correctness property was stashed in KB"
    | Missing_raw_ir -> "Raw Ir compiled from core_theory not found in KB"
    | Missing_semantics s -> Format.sprintf "Semantics for %s not found in KB" s
    | Command_not_found s -> s
    | Patch_code_not_parsed s -> "Failed to parse patch code: " ^ s
    | Incorrect_patch_point s ->
      Format.sprintf
        "Provided patch point %s is not an executable location" s
    | Higher_vars_not_substituted s ->
      "Failed to substitute higher variables into patch code: " ^ s
    | Exit_code s -> s
    | Unexpected_exit s -> s
    | WP_result_unknown s -> s
    | Max_tries n -> Format.sprintf "Tried %d times. Giving up" n
    | Minizinc_deserialization s -> s
    | Core_c_error s ->
      Format.sprintf "Core C failed with error: %s" s
    | Other s -> s
  in
  Format.fprintf ppf "@[%s@]@." msg

(* Encorprate these errors into the KB error type (a conflict). *)
type KB.Conflict.t += Problem of t
let printer (e : KB.Conflict.t) =
  match e with
  | Problem err -> Some (Format.asprintf "%a" pp err)
  | _ -> failwith "Unexpected Conflict Type"
let () = KB.Conflict.register_printer printer

(* Report an error and fail. *)
let fail e =
  let msg = Format.asprintf "%a" pp e in
  Events.(send @@ Info msg);
  KB.fail (Problem e)
