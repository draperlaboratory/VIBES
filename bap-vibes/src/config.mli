(** Encapsulates the configuration needed to run the VIBES pipeline. *)

open !Core_kernel

module Hvar = Higher_var

(** A type to represent an individual patch fragment. *)
type patch

(** A type to represent a configuration record. *)
type t

(** [patch_name p] returns the name of the patch [p]. *)
val patch_name : patch -> string

(** [patch_code p] returns the patch code for the patch [p]. *)
val patch_code : patch -> Sexp.t list

(** [patch_point p] returns the start address for the patch [p]. *)
val patch_point : patch -> Bitvec.t

(** [patch_size p] returns the number of bytes to replace with patch [p]. *)
val patch_size : patch -> int

(** [patch_vars p] returns the higher vars declared for the patch [p]. *)
val patch_vars : patch -> Hvar.t list

(** [exe config] returns the filepath of the original exe to patch. *)
val exe : t -> string

(** [patch config] returns the list of patch fragments *)
val patches : t -> patch list

(** [func config] returns the name of the function to verify. *)
val func : t -> string

(** [property config] returns the correctness property to use to verify
    whether the patched exe is correct. *)
val property : t -> Sexp.t

(** [patched_exe_filepath config] returns the optional user-specified output
    location. *)
val patched_exe_filepath : t -> string option

(** [max_tries config] returns the max allowed number of CEGIS iterations. *)
val max_tries : t -> int option

(** [minizinc_model_filepath config] returns the path to the minizinc model. *)
val minizinc_model_filepath : t -> string

(** [pp ppf config] is a pretty printer for a configuration record. *)
val pp : Format.formatter -> t -> unit

(** [create ~patch_name ~patch_code ~patch_point ~patch_size ~patch_vars]
    will create a patch record, where:
    - [~patch_name] is the name of the patch
    - [~patch_code] is the code of the patch
    - [~patch_point] is the addres in the original exe to start patching at
    - [~patch_size] is the number of bytes to replace in the original exe 
    - [~patch_vars] are higher variables declared for the patch *)
val create_patch : patch_name:string -> patch_code:Sexp.t list ->
  patch_point:Bitvec.t -> patch_size:int -> patch_vars:Hvar.t list -> patch

(** [create ~exe ~config_filepath ~patched_exe_filepath
    ~minizinc_model_filepath] will create a configuration record, where:
    - [~exe] is the filepath to the original exe
    - [~patches] is a list of [patch] records
    - [~func] is the name of the function to check for correctness
    - [~property] is the correctness property to validate
    - [~patched_exe_filepath] is the optional output location 
    - [~max_tries] is the optional number of tries to allow
    - [~minizinc_model_filepath] is the minizinc model file location *)
val create : exe:string -> patches:patch list -> func:string ->
  property:Sexp.t -> patched_exe_filepath:string option ->
  max_tries : int option -> minizinc_model_filepath:string -> t
