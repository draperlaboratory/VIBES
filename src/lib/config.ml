(* Implements {!Config}. *)

open !Core_kernel
open Bap.Std
open Monads.Std

(* Encapsulate configuration errors. *)
module Errors = struct

  type t =
    | Missing_exe
    | Missing_patch
    | Missing_patch_point
    | Missing_property
    | Invalid_size of string
    | Invalid_hex of string
    | Invalid_property of string

  let pp (ppf : Format.formatter) t : unit =
    let msg = match t with
      | Missing_exe -> "missing the filepath of an executable"
      | Missing_patch -> "missing a patch name"
      | Missing_patch_point -> "missing a patch point"
      | Missing_property -> "missing a correctness property"
      | Invalid_size desc -> desc
      | Invalid_hex desc -> desc
      | Invalid_property desc -> desc
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

(* Parse a hex string into a bitvector, or error. *)
let validate_patch_point (value : string) : (Bitvec.t, error) Stdlib.result =
  try 
    Err.return (Bitvec.of_string value)
  with Invalid_argument _ ->
    let msg = Format.sprintf "Invalid hex string: %s" value in
    Err.fail (Errors.Invalid_hex msg)

(* Parse an string into an S-expression, or error. *)
let validate_property (value : string) : (Sexp.t, error) Stdlib.result =
  try
    Err.return (Sexp.of_string value)
  with Failure _ ->
    let msg = Format.sprintf "Invalid S-expression: %s" value in
    Err.fail (Errors.Invalid_property msg)

(* Construct a configuration record from the given parameters. *)
let create ~exe:(exe : string) ~patch:(patch : string)
    ~patch_point:(patch_point : string) ~patch_size:(patch_size : int)
    ~property:(property : string)
    ~patched_exe_filepath:(patched_exe_filepath : string option)
    ~max_tries:(max_tries : int option)
    : (t, error) result =
  is_not_empty exe Errors.Missing_exe >>= fun exe ->
  is_not_empty patch Errors.Missing_patch >>= fun patch ->
  is_not_empty patch_point Errors.Missing_patch_point >>= fun patch_point ->
  is_not_empty property Errors.Missing_property >>= fun property ->
  validate_patch_point patch_point >>= fun patch_point ->
  validate_property property >>= fun property ->
  let record = { exe; patch; patch_point; patch_size; property; 
    patched_exe_filepath; max_tries } in
  Ok record
