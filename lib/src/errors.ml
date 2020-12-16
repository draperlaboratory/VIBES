(* Implements {!Errors}. *)

open Bap_knowledge

module KB = Knowledge

(* Errors we want to raise explicitly. *)
type t =
  | Failed_to_load_proj of string
  | Not_implemented of string
  | Missing_patch_name
  | Missing_assembly
  | Missing_original_exe_filepath
  | Missing_original_exe_prog
  | Missing_addr_size
  | Missing_patched_exe_filepath
  | Missing_tmp_patched_exe_filepath
  | Missing_patch_point
  | Missing_patch_size
  | Missing_property
  | Command_not_found of string
  | Exit_code of string
  | Unexpected_exit of string
  | WP_result_unknown of string
  | Max_tries of int
  | Other of string

(* A pretty-printer for these errors. *)
let pp ppf (e : t) =
  let msg = match e with
    | Failed_to_load_proj s -> s
    | Not_implemented s -> s
    | Missing_patch_name -> "No patch name was stashed in KB"
    | Missing_assembly -> "No patch assembly was stashed in KB"
    | Missing_original_exe_filepath ->
      "No filepath for the original exe was stashed in KB"
    | Missing_original_exe_prog ->
      "No lifted program for the original exe was stashed in KB"
    | Missing_addr_size -> "No address size was stashed in KB"
    | Missing_patched_exe_filepath ->
      "No filepath for the patched exe was stashed in KB"
    | Missing_tmp_patched_exe_filepath ->
      "No filepath for the temporary patched exe was stashed in KB"
    | Missing_patch_point -> "No patch point was stashed in KB"
    | Missing_patch_size -> "No patch size was stashed in KB"
    | Missing_property -> "No correctness property was stashed in KB"
    | Command_not_found s -> s
    | Exit_code s -> s
    | Unexpected_exit s -> s
    | WP_result_unknown s -> s
    | Max_tries n -> Format.sprintf "Tried %d times. Giving up" n 
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
