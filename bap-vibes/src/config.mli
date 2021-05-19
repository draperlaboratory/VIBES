(** Encapsulates the configuration needed to run the VIBES pipeline. *)

open! Core_kernel

type patch
(** A type to represent an individual patch fragment. *)

type t
(** A type to represent a configuration record. *)

val patch_name : patch -> string
(** [patch_name p] returns the name of the patch [p]. *)

val patch_code : patch -> Sexp.t list
(** [patch_code p] returns the patch code for the patch [p]. *)

val patch_point : patch -> Bitvec.t
(** [patch_point p] returns the start address for the patch [p]. *)

val patch_size : patch -> int
(** [patch_size p] returns the number of bytes to replace with patch [p]. *)

val exe : t -> string
(** [exe config] returns the filepath of the original exe to patch. *)

val patches : t -> patch list
(** [patch config] returns the list of patch fragments *)

val func : t -> string
(** [func config] returns the name of the function to verify. *)

val property : t -> Sexp.t
(** [property config] returns the correctness property to use to verify
    whether the patched exe is correct. *)

val patched_exe_filepath : t -> string option
(** [patched_exe_filepath config] returns the optional user-specified output
    location. *)

val max_tries : t -> int option
(** [max_tries config] returns the max allowed number of CEGIS iterations. *)

val minizinc_model_filepath : t -> string
(** [minizinc_model_filepath config] returns the path to the minizinc model. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf config] is a pretty printer for a configuration record. *)

val create_patch :
  patch_name:string ->
  patch_code:Sexp.t list ->
  patch_point:Bitvec.t ->
  patch_size:int ->
  patch
(** [create ~patch_name ~patch_code ~patch_point ~patch_size]
    will create a patch record, where:
    - [~patch_name] is the name of the patch
    - [~patch_code] is the code of the patch
    - [~patch_point] is the addres in the original exe to start patching at
    - [~patch_size] is the number of bytes to replace in the original exe *)

val create :
  exe:string ->
  patches:patch list ->
  func:string ->
  property:Sexp.t ->
  patched_exe_filepath:string option ->
  max_tries:int option ->
  minizinc_model_filepath:string ->
  t
(** [create ~exe ~config_filepath ~patched_exe_filepath
    ~minizinc_model_filepath] will create a configuration record, where:
    - [~exe] is the filepath to the original exe
    - [~patches] is a list of [patch] records
    - [~func] is the name of the function to check for correctness
    - [~property] is the correctness property to validate
    - [~patched_exe_filepath] is the optional output location 
    - [~max_tries] is the optional number of tries to allow
    - [~minizinc_model_filepath] is the minizinc model file location *)
