(* Configuration/parameters needed to run the VIBES pipeline. *)

open !Core_kernel
open Monads.Std

(* Encapsulate configuration errors. *)
module Errors : sig

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
    | Invalid_max_tries

  val pp : Format.formatter -> t -> unit

end

(* Error results are derived from the {!Errors} module. *)
type error = Errors.t Monad.Result.Make (Errors) (Monad.Ident).error

(* A type to represent a configuration record. *)
type t

(* A type to represent individual patch fragments. *)
type patch =
  {
    (* The name of the patch to use. *)
    patch_name : string;

    (* An s-expression version of the patch's core theory code *)
    patch_code : string;

    (* The address in the original exe to start patching from. *)
    patch_point : Bitvec.t;

    (* The number of bytes of code that the patch replaces or removes,
       beginning at the patch_point *)
    patch_size : int
  }

(* [exe config] returns the filepath of the original exe to patch. *)
val exe : t -> string

(* [patch config] returns the list of patch fragments *)
val patches : t -> patch list

(* [func config] returns the name of the function to verify. *)
val func : t -> string

(* [property config] returns the correctness property to use to verify
   whether the patched exe is correct. *)
val property : t -> Sexp.t

(* [patched_exe_filepath config] returns the optional user-specified output
   location. *)
val patched_exe_filepath : t -> string option

(* [max_tries config] returns the max allowed number of CEGIS iterations. *)
val max_tries : t -> int option

(* [pp ppf config] is a pretty printer for a configuration record. *)
val pp : Format.formatter -> t -> unit

(* [create ~exe ~config_filepath ~patched_exe_filepath]
   will create a configuration record, where:
   - [~exe] is the filepath to the original exe
   - [~config_filepath] is the filepath to the config json file
   - [~patched_exe_filepath] is the optional output location *)
val create : exe:string -> config_filepath:string ->
  patched_exe_filepath:string option -> (t, error) result
