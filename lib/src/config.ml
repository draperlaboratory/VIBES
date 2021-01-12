(* Implements {!Config}. *)

open !Core_kernel
open Monads.Std
module Json = Yojson.Safe

(* Encapsulate configuration errors. *)
module Errors = struct

  type t =
    | Missing_exe
    | Missing_patch
    | Missing_patch_point
    | Missing_property
    | Missing_size
    | Config_not_parsed of string
    | Invalid_hex of string
    | Invalid_property of string
    | Invalid_max_tries

  let pp (ppf : Format.formatter) t : unit =
    let msg = match t with
      | Missing_exe -> "missing the filepath of an executable"
      | Missing_patch ->
         "config json field \"patch\" must be a non-empty string"
      | Missing_patch_point ->
         "config json field \"patch-point\" must be a non-empty string"
      | Missing_property ->
         "config json field \"property\" must be a non-empty string"
      | Missing_size ->
         "config json field \"patch-size\" must be an integer"
      | Config_not_parsed s ->
         "error finding or parsing config JSON file: " ^ s
      | Invalid_hex desc -> desc
      | Invalid_property desc -> desc
      | Invalid_max_tries ->
         "optional config json field \"max-tries\" must be an integer"
    in
    Format.fprintf ppf "@[%s@]" msg

end

(* Monadize the errors. *)
module Err = Monad.Result.Make (Errors) (Monad.Ident)
open Err.Syntax
type error = Errors.t Err.error

(* The configuration for a run of the VIBES pipeline. *)
type t = {
  exe : string; (* The filename (path) of the executable to patch. *)
  patch : string; (* The name of the patch to use. *)
  patch_point : Bitvec.t; (* The address to start patching at. *)
  patch_size : int; (* The number of bytes to overwrite. *)
  property : Sexp.t; (* Correctness property. *)
  patched_exe_filepath : string option; (* Optional output location *)
  max_tries : int option; (* Optional number of CEGIS iterations to allow *)
}

(* Accessors. *)
let exe t : string = t.exe
let patch t : string = t.patch
let patch_point t : Bitvec.t = t.patch_point
let patch_size t : int = t.patch_size
let property t : Sexp.t = t.property
let patched_exe_filepath t : string option = t.patched_exe_filepath
let max_tries t : int option = t.max_tries

(* For printing configuration. *)
let pp (ppf : Format.formatter) t : unit =
  let info = String.concat ~sep:"\n" [
      Printf.sprintf "Exe: %s" t.exe;
      Printf.sprintf "Patch: %s" t.patch;
      Printf.sprintf "Patch_point: %s" (Bitvec.to_string t.patch_point);
      Printf.sprintf "Patch_size: %d" t.patch_size;
      Printf.sprintf "Property: %s" (Sexp.to_string t.property);
      Printf.sprintf "Output filepath: %s"
        (Option.value t.patched_exe_filepath ~default:"none provided");
      Printf.sprintf "Max tries: %d" (Option.value t.max_tries ~default:0);
    ] in
  Format.fprintf ppf "@[%s@]@." info

(* Error if a string is empty. *)
let is_not_empty (value : string) (e : Errors.t)
  : (string, error) Stdlib.result =
  match String.length value with
  | 0 -> Err.fail e
  | _ -> Err.return value

(* Extract the patch name and check it is non-empty string. *)
let validate_patch (obj : Json.t) : (string, error) Stdlib.result =
  match Json.Util.member "patch" obj with
  | `String s ->
     if String.length s = 0 then Err.fail Errors.Missing_patch
     else Err.return s
  | _ -> Err.fail Errors.Missing_patch

(* Extract the patch point field and parse the hex string into a bitvector, or
   error. *)
let validate_patch_point (obj : Json.t) : (Bitvec.t, error) Stdlib.result =
  match Json.Util.member "patch-point" obj with
  | `String s ->
     begin
       try
         Err.return (Bitvec.of_string s)
       with Invalid_argument _ ->
         let msg = Format.sprintf "Invalid hex string: %s" s in
         Err.fail (Errors.Invalid_hex msg)
     end
  | _ -> Err.fail Errors.Missing_patch_point

(* Extract the patch size integer, or error. *)
let validate_patch_size (obj : Json.t) : (int, error) Stdlib.result =
  match Json.Util.member "patch-size" obj with
  | `Int i -> Err.return i
  | _ -> Err.fail Errors.Missing_size

(* Extract the property field string and parse it into an S-expression, or
   error. *)
let validate_property (obj : Json.t) : (Sexp.t, error) Stdlib.result =
  match Json.Util.member "property" obj with
  | `String s ->
     begin
       try
         Err.return (Sexp.of_string s)
       with Failure _ ->
         let msg = Format.sprintf "Invalid S-expression: %s" s in
         Err.fail (Errors.Invalid_property msg)
     end
  | _ -> Err.fail Missing_property

let validate_max_tries (obj : Json.t) : (int option, error) Stdlib.result =
  match Json.Util.member "max-tries" obj with
  | `Int i -> Err.return (Some i)
  | `Null -> Err.return None
  | _ -> Err.fail Invalid_max_tries

(* Parse the user-provided JSON config file into a Yojson.Safe.t *)
let parse_json (config_filepath : string) : (Json.t, error) Stdlib.result =
  try
    Err.return (Json.from_file config_filepath)
  with e -> Err.fail (Errors.Config_not_parsed (Exn.to_string e))

(* Construct a configuration record from the given parameters. *)
let create ~exe:(exe : string) ~config_filepath:(config_filepath : string)
      ~patched_exe_filepath:(patched_exe_filepath : string option)
    : (t, error) result =
  is_not_empty exe Errors.Missing_exe >>= fun exe ->
  parse_json config_filepath >>= fun config_json ->
  validate_patch config_json >>= fun patch ->
  validate_patch_point config_json >>= fun patch_point ->
  validate_patch_size config_json >>= fun patch_size ->
  validate_property config_json >>= fun property ->
  validate_max_tries config_json >>= fun max_tries ->
  let record = { exe; patch; patch_point; patch_size; property;
                 patched_exe_filepath; max_tries } in
  Ok record
