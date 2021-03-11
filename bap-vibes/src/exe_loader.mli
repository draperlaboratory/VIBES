(* A loader to lift an executable into a BAP {Project.t}. *)

open Bap.Std
open Bap_knowledge

module KB = Knowledge

(* [load "/path/to/exe"] loads the executable at [/path/to/exe]. *)
val simple_load : string -> project Core_kernel.Or_error.t 

(* [load "/path/to/exe"] loads the executable at [/path/to/exe],
   inside a KB monadic computation. *)
val load : string -> Project.t KB.t
