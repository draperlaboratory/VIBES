(* Implements {!Vibes_plugin_parameters}. *)

open !Core_kernel
open Monads.Std

module Json = Yojson.Safe
module Vibes_config = Bap_vibes.Config
module Parse_c = Bap_vibes.Parse_c
module Errors = Vibes_plugin_errors

(* Monadize the errors. *)
module Err = Monad.Result.Make (Errors) (Monad.Ident)
open Err.Syntax
type error = Errors.t Err.error

(* Get the default minizinc model filepath. *)
let minizinc_model_filepath = Vibes_plugin_constants.minizinc_model_filepath

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

(* Extract the patch code, check it's a non-empty string, and parse
   into a [Sexp.t list] (it should be a valid S-expression). *)
let validate_patch_code (nm : string) (obj : Json.t)
    : (Sexp.t list, error) Stdlib.result =
  match Json.Util.member "patch-code" obj with
  | `String s ->
    begin
     if String.length s = 0 then Err.fail Errors.Missing_patch_code
     else 
       begin
         let s = match Parse_c.c_patch_to_sexp_string s with
                 | Ok s1 -> s1
                 | Error _ -> s
         in
         try Err.return (Sexp.scan_sexps (Lexing.from_string s))
         with _ -> 
           let msg =
             "Code for patch '" ^ nm ^ "' is not a valid S-expression" in
           Err.fail (Errors.Invalid_patch_code msg)
       end
    end
  | _ -> Err.fail Errors.Missing_patch_code

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
let validate_patch (obj : Json.t) 
    : (Vibes_config.patch, error) Stdlib.result =
  validate_patch_name obj >>= fun patch_name ->
  validate_patch_code patch_name obj >>= fun patch_code ->
  validate_patch_point obj >>= fun patch_point ->
  validate_patch_size obj >>= fun patch_size ->
  let p = Vibes_config.create_patch
    ~patch_name ~patch_code ~patch_point ~patch_size in
  Err.return p

(* Extract and validate the patch fragment list, or error. *)
let validate_patches (obj : Json.t)
    : (Vibes_config.patch list, error) Stdlib.result =
  match Json.Util.member "patches" obj with
  | `List ps -> Err.all (List.map ~f:validate_patch ps)
  | _ -> Err.fail Errors.Missing_patches

(* Extract and validate the name of the function to verify. *)
let validate_func (obj : Json.t) : (string, error) Stdlib.result =
  match Json.Util.member "func" obj with
  | `String s ->
    begin
      if (String.length s) > 0 then Err.return s
      else Err.fail Errors.Missing_func
    end
  | _ -> Err.fail Errors.Missing_func

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
  | _ -> Err.fail Errors.Missing_property

(* Extract the max-tries value, and make sure it's an [int] (if provided). *)
let validate_max_tries (obj : Json.t) : (int option, error) Stdlib.result =
  match Json.Util.member "max-tries" obj with
  | `Int i -> Err.return (Some i)
  | `Null -> Err.return None
  | _ -> Err.fail Errors.Invalid_max_tries

(* Extract the minizinc model filepathi value (or use the default), and make
   sure the file really exists. *)
let validate_minizinc_model_filepath (obj : Json.t)
    : (string, error) Stdlib.result = 
  match Json.Util.member "minizinc-model" obj with
  | `String s ->
    begin
      if (String.length s) > 0 then
        begin
          let realpath = Vibes_plugin_utils.realpath s in
          match realpath with
          | Ok path -> Err.return path
          | Error e -> Err.fail e
        end
      else Err.fail (Errors.Missing_minizinc_model_filepath)
    end
  | _ ->
    begin
      let realpath = Vibes_plugin_utils.realpath minizinc_model_filepath in
      match realpath with
      | Ok path -> Err.return path
      | Error e -> Err.fail e
    end

(* Parse the user-provided JSON config file into a Yojson.Safe.t *)
let parse_json (config_filepath : string) : (Json.t, error) Stdlib.result =
  try
    Err.return (Json.from_file config_filepath)
  with e -> Err.fail (Errors.Config_not_parsed (Exn.to_string e))

(* Construct a configuration record from the given parameters. *)
let create ~exe:(exe : string) ~config_filepath:(config_filepath : string)
      ~patched_exe_filepath:(patched_exe_filepath : string option)
    : (Vibes_config.t, error) result =
  is_not_empty exe Errors.Missing_exe >>= fun exe ->
  parse_json config_filepath >>= fun config_json ->
  validate_patches config_json >>= fun patches ->
  validate_func config_json >>= fun func ->
  validate_property config_json >>= fun property ->
  validate_max_tries config_json >>= fun max_tries ->
  validate_minizinc_model_filepath config_json >>=
    fun minizinc_model_filepath ->
  let result = Vibes_config.create 
    ~exe ~patches ~func ~property ~patched_exe_filepath ~max_tries
    ~minizinc_model_filepath in
  Ok result
