(** Implements {!Higher_var}. *)

open Core

module Utils = Vibes_utils

type memory =
  | Frame of string * Utils.Json.Bitvector.t [@name "frame"]
  | Global of Utils.Json.Bitvector.t [@name "address"]
[@@deriving yojson, equal, compare]

type value =
  | Constant of Utils.Json.Bitvector.t [@name "constant"]
  | Registers of {
      at_entry: string option [@yojson.option] [@key "at-entry"];
      at_exit : string option [@yojson.option] [@key "at-exit"];
      allow_opt : bool [@default false] [@key "allow-opt"];
    } [@name "register"]
  | Preassign of string [@name "preassign"]
  | Memory of memory [@name "memory"]
[@@deriving yojson, equal, compare]

let default_value : value = Registers {
    at_entry = None;
    at_exit = None;
    allow_opt = false;
  }

type t = {
  name : string;
  value : value [@default default_value] [@key "storage-class"];
} [@@deriving yojson, equal, compare]

let find (name : string) (vars : t list) : t option =
  List.find vars ~f:(fun t -> String.equal name t.name)
