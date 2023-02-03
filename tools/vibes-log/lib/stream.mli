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

(** A message. *)
type event = string

(** A handler for the message. *)
type observer = event -> unit

(** The formatter for messages. *)
type 'a formatter = ('a, Format.formatter, unit, unit) format4

(** Adds an observer to the logging stream. *)
val subscribe : observer -> unit

(** Formats and sends a message to all observers. *)
val send : 'a formatter -> 'a
