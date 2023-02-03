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

(** Sets up the verbosity of the logging output.

    - [verbose] enables verbose logging.
    - [no_color] disables TTY color output (only plain ASCII
      will appear in the output).
*)
val setup : verbose:bool -> no_color:bool -> unit

(** Enables verbose logging. *)
val verbose : bool Cmdliner.Term.t

(** Disables TTY color output. *)
val no_color : bool Cmdliner.Term.t
