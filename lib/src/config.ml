(* Implements {!Config}. *)

open !Core_kernel
open Monads.Std
module Json = Yojson.Safe

(* Encapsulate configuration errors. *)
module Errors = struct

  type t =
    | Missing_exe
    | Missing_patches
    | Missing_patch_name
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
      | Missing_patches ->
         "config json field \"patches\" is mandatory and must be a json array"
      | Missing_patch_name ->
           "each patch in the config json \"patches\" list must have a "
         ^ "\"patch-name\" field containing a non-empty string"
      | Missing_patch_point ->
           "each patch in the config json \"patches\" list must have a "
         ^ "\"patch-point\" field containing a non-empty string"
      | Missing_property ->
         "config json field \"property\" must be a non-empty string"
      | Missing_size ->
           "each patch in the config json \"patches\" list must have a "
         ^ "\"patch-size\" field containing an integer"
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

(* A type to represent individual patch fragments. *)
type patch =
  {
    (* The name of the patch to use. *)
    patch_name : string;

    (* The address in the original exe to start patching from. *)
    patch_point : Bitvec.t;

    (* The number of bytes of code that the patch replaces or removes,
       beginning at the patch_point *)
    patch_size : int
  }

(* The configuration for a run of the VIBES pipeline. *)
type t = {
  exe : string; (* The filename (path) of the executable to patch. *)
  patches : patch list; (* The list of patches to apply. *)
  property : Sexp.t; (* Correctness property. *)
  patched_exe_filepath : string option; (* Optional output location *)
  max_tries : int option; (* Optional number of CEGIS iterations to allow *)
}

(* Accessors. *)
let exe t : string = t.exe
let patches t : patch list = t.patches
let property t : Sexp.t = t.property
let patched_exe_filepath t : string option = t.patched_exe_filepath
let max_tries t : int option = t.max_tries

(* For printing configuration. *)
let patch_to_string (p : patch) : string =
  String.concat ~sep:"\n" [
      Printf.sprintf "  {Patch_name: %s" p.patch_name;
      Printf.sprintf "   Patch_point: %s" (Bitvec.to_string p.patch_point);
      Printf.sprintf "   Patch_size: %d}" p.patch_size;
    ]

let patches_to_string (ps : patch list) : string =
  String.concat ~sep:",\n" (List.map ~f:patch_to_string ps)

let pp (ppf : Format.formatter) t : unit =
  let info = String.concat ~sep:"\n" [
      Printf.sprintf "Exe: %s" t.exe;
      Printf.sprintf "Patches: %s" (patches_to_string t.patches);
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
let validate_patch_name (obj : Json.t) : (string, error) Stdlib.result =
  match Json.Util.member "patch-name" obj with
  | `String s ->
     if String.length s = 0 then Err.fail Errors.Missing_patch_name
     else Err.return s
  | _ -> Err.fail Errors.Missing_patch_name

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

(* Validate a specific patch fragment within the list, or error *)
let validate_patch (obj : Json.t) : (patch, error) Stdlib.result =
  validate_patch_name obj >>= fun patch_name ->
  validate_patch_point obj >>= fun patch_point ->
  validate_patch_size obj >>= fun patch_size ->
  Err.return { patch_name; patch_point; patch_size }

(* Extract and validate the patch fragment list, or error. *)
let validate_patches (obj : Json.t) : (patch list, error) Stdlib.result =
  match Json.Util.member "patches" obj with
  | `List ps -> Err.all (List.map ~f:validate_patch ps)
  | _ -> Err.fail Errors.Missing_patches

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
  validate_patches config_json >>= fun patches ->
  validate_property config_json >>= fun property ->
  validate_max_tries config_json >>= fun max_tries ->
  let record = { exe; patches; property; patched_exe_filepath; max_tries } in
  Ok record
