open Core

type event = string
type observer = event -> unit
type 'a formatter = ('a, Format.formatter, unit, unit) format4

let observers : (int, observer) Hashtbl.t = Hashtbl.create (module Int)

let subscribe (observer : observer) : unit =
  let next_slot = Hashtbl.length observers in
  Hashtbl.set observers ~key:next_slot ~data:observer

let broadcast (event : event) : unit =
  Hashtbl.iter observers ~f:((|>) event)

let send (fmt : 'a formatter) : 'a =
  Format.kasprintf broadcast fmt
