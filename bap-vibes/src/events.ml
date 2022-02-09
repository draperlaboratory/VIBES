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

(* Implements {!Events}. *)

open Bap_future.Std

type t =
  | Header of string
  | Info of string
  | Rule

let (events, signal) : (t stream * t signal) = Stream.create ()

let subscribe (observer : t -> unit) : unit =
  Stream.observe events observer

let send event = Signal.send signal event
