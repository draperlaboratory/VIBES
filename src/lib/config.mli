(* Configuration/parameters needed to run the VIBES pipeline. *)

open !Core_kernel
open Bap.Std
open Monads.Std

(* Encapsulate configuration errors. *)
module Errors : sig

  type t =
    | Missing_exe
    | Missing_patch
    | Missing_patch_point
    | Missing_property
    | Invalid_size of string
    | Invalid_hex of string
    | Invalid_property of string

  val pp : Format.formatter -> t -> unit

end

(* Error results are derived from the {!Errors} module. *)
type error = Errors.t Monad.Result.Make (Errors) (Monad.Ident).error

(* A type to represent a configuration record. *)
type t

(* [exe config] returns the filepath of the original exe to patch. *)
val exe : t -> string

(* [patch config] returns the name of the patch to use. *)
val patch : t -> string

(* [patch_point config] returns the address in the original exe to start
   patching from. *)
val patch_point : t -> Bitvec.t

(* [patch_size config] returns the number of bytes in the original exe
   that need to be patched. *)
val patch_size : t -> int

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

(* [create ~exe ~patch ~patch_point ~patch_size ~property
           ~patched_exe_filepath ~max_tries]
   will create a configuration record, where:
   - [~exe] is the filepath to the original exe
   - [~patch] is the name of the patch to use
   - [~patch_point] (a hex string) is the address to start patching from
   - [~patch_size] is the number of bytes to patch
   - [~property] is the correctness property (to verify the patched exe)
   - [~patched_exe_filepath] is the optional output location
   - [~max_tries] is the number of CEGIS iterations to allow *)
val create : exe:string -> patch:string -> patch_point:string ->
  patch_size:int -> property:string -> patched_exe_filepath:string option ->
  max_tries:int option -> (t, error) result
