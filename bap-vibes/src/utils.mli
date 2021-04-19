(** Common utilities. *)

open Bap.Std
open Bap_knowledge
open Bap_core_theory
module KB = Knowledge

(** [cp src dst] copies the file from the [src] to the [dst] filepath. *)
val cp : string -> string -> unit

(** [run_process command args] runs [command] with [args] *)
val run_process : string -> string list -> (unit, Kb_error.t) Result.t

(** [lift_kb_result] transforms a computation in the Result.t monad
    into the KB.t monad. *)
val lift_kb_result : ('a, Kb_error.t) Result.t -> 'a KB.t

(** [load_exe "/path/to/exe"] loads /path/to/exe into BAP. *)
val load_exe : string -> (project * Program.t, Toplevel_error.t) result

(** [get_sub prog "main"] returns the function ["main"] in [prog], if present. *)
val get_func : Program.t -> string -> Sub.t option

(** [get_lang patch] returns the language associated with the
   patch in the current binary under scutiny, at the location
   indicated at the patch. *)
val get_lang : filename:string -> addr_size:int -> addr:Bitvec.t -> Theory.language KB.t


(** [get_lang patch] returns the target associated with the
   patch in the current binary under scutiny, at the location
   indicated at the patch. *)
val get_target : filename:string -> addr_size:int -> addr:Bitvec.t -> Theory.target KB.t
