(** Implements {!Higher_var}. *)

open !Core_kernel
open Bap.Std

type register = string
type offset = word

let equal_word = Bitvector.equal

type stored_in =
  | Register of string
  | Memory of string * word
[@@deriving equal, compare]

type t = {
  name : string;
  at_entry : stored_in;
  at_exit : stored_in;
} [@@deriving equal, compare]

let name t = t.name
let at_entry t = t.at_entry
let at_exit t = t.at_exit

let create (name : register) (at_entry : stored_in) (at_exit : stored_in) : t =
  { name; at_entry; at_exit; }

let is_reg (v : stored_in) : bool =
  match v with
  | Register _ -> true
  | _ -> false

let find (name : register) (vars : t list) : t option =
  List.find vars ~f:(fun t -> String.equal name t.name)
