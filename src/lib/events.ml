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
