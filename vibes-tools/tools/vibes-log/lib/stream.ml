open Core

type event = string
type observer = event -> unit

let observers : (int, observer) Hashtbl.t = Hashtbl.create (module Int)

let subscribe (observer : observer) : unit =
  let next_slot = Hashtbl.length observers in
  Hashtbl.set observers ~key:next_slot ~data:observer

let send (event : event) : unit =
  Hashtbl.iter observers ~f:(fun handle -> handle event)
