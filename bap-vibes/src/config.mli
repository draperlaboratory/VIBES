(** Encapsulates the configuration needed to run the VIBES pipeline. *)

open !Core_kernel

module Hvar = Higher_var

module Wp_params = Bap_wp.Run_parameters

(** A type to represent an individual patch fragment. *)
type patch

(** A type to represent a configuration record. *)
type t

(** A type to represent patch cody which may either be C or literal assembly *)
type patch_code = CCode of Cabs.definition | ASMCode of string

(** A type to represent known regions that may be overwritten with patch code *)
type patch_space = {
    space_offset : int64;
    space_size : int64
  }

(** [patch_name p] returns the name of the patch [p]. *)
val patch_name : patch -> string

(** [patch_code p] returns the patch code for the patch [p]. *)
val patch_code : patch -> patch_code

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

(** [patched_exe_filepath config] returns the optional user-specified output
    location. *)
val patched_exe_filepath : t -> string option

(** [max_tries config] returns the max allowed number of CEGIS iterations. *)
val max_tries : t -> int option

(** [minizinc_model_filepath config] returns the path to the minizinc model. *)
val minizinc_model_filepath : t -> string

(** [ogre config] returns the contents of the user-provided ogre file for use in
   the raw loader, if provided *)
val ogre : t -> string option

(** [wp_params config] returns the input parameters for the invocation to WP *)
val wp_params : t -> Wp_params.t

(** [patch_spaces config] returns known dead regions suitable for patch
   placement *)
val patch_spaces : t -> patch_space list

(** [pp ppf config] is a pretty printer for a configuration record. *)
val pp : Format.formatter -> t -> unit

(** [create_patch ~patch_name ~patch_code ~patch_point ~patch_size ~patch_vars]
    will create a patch record, where:
    - [~patch_name] is the name of the patch
    - [~patch_code] is the code of the patch
    - [~patch_point] is the addres in the original exe to start patching at
    - [~patch_size] is the number of bytes to replace in the original exe
    - [~patch_vars] are higher variables declared for the patch *)
val create_patch :
  patch_name:string
  -> patch_code:patch_code
  -> patch_point:Bitvec.t
  -> patch_size:int
  -> patch_vars:Hvar.t list
  -> patch

(** [create ~exe ~config_filepath ~patched_exe_filepath
    ~minizinc_model_filepath] will create a configuration record, where:
    - [~exe] is the filepath to the original exe
    - [~patches] is a list of [patch] records
    - [~patched_exe_filepath] is the optional output location
    - [~max_tries] is the optional number of tries to allow
    - [~minizinc_model_filepath] is the minizinc model file location
    - [~ogre] is the optional filepath to an ogre file
    - [~patch_spaces] is the list of known empty regions for patch code (which
        may be empty)
    - [~wp_params] is the parameter struct for WP *)
val create :
  exe:string
  -> patches:patch list
  -> patched_exe_filepath:string option
  -> max_tries : int option
  -> minizinc_model_filepath:string
  -> ogre:string option
  -> patch_spaces:patch_space list
  -> wp_params:Wp_params.t
  -> t
