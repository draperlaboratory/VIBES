(***************************************************************************)
(*                                                                         *)
(*  Copyright (C) 2022/2023 The Charles Stark Draper Laboratory, Inc.      *)
(*                                                                         *)
(*  This file is provided under the license found in the LICENSE file in   *)
(*  the top-level directory of this project.                               *)
(*                                                                         *)
(*  This research was developed with funding from the Defense Advanced     *)
(*  Research Projects Agency (DARPA).                                      *)
(*                                                                         *)
(***************************************************************************)

(** Common utilities. *)

open Result
open Bap.Std
open Bap_knowledge
open Bap_core_theory
module KB = Knowledge

(** [cp src dst] copies the file from the [src] to the [dst] filepath. *)
val cp : string -> string -> unit

(** [run_process command args] runs [command] with [args] *)
val run_process : string -> string list -> (unit, Kb_error.t) result

(** [lift_kb_result] transforms a computation in the Result.t monad
    into the KB.t monad. *)
val lift_kb_result : ('a, Kb_error.t) result -> 'a KB.t

(** [load_exe "/path/to/exe"] loads /path/to/exe into BAP. *)
val load_exe : string -> (project * Program.t, Toplevel_error.t) result

(** [get_sub prog "main"] returns the function ["main"] in [prog], if present. *)
val get_func : Program.t -> string -> Sub.t option

(** [get_lang patch] returns the language associated with the
    patch in the current binary under scutiny, at the location
    indicated at the patch. *)
val get_lang : addr:Bitvec.t -> Theory.language KB.t


(** [get_lang patch] returns the target associated with the
    patch in the current binary under scutiny, at the location
    indicated at the patch. *)
val get_target : addr:Bitvec.t -> Theory.target KB.t

(** [print_c Cprint.print_foo foo] will correctly invoke an
    appropriate FrontC printer to print to a string buffer, and free
    the ressources involved.  Observationally, it simply invokes the
    first argument on the second, and returns whatever string is in the
    [Cprint.out] channel.  *)
val print_c : ('a -> unit) -> 'a -> string

(** [dedup_list_stable l ~compare] removes all duplicates from [l] according
    to [compare] while preserving the original order in [l]. *)
val dedup_list_stable : 'a list -> compare:('a -> 'a -> int) -> 'a list
