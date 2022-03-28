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

(** Implements {!Higher_var}. *)

open !Core_kernel
open Bap.Std

let equal_word = Bitvector.equal

(* For a `Frame`, we have the name of the frame pointer register
   and an offset. *)
type memory =
  | Frame of string * word
  | Global of word
[@@deriving equal, compare]

type t = {
  name : string;
  value : value;
} [@@deriving equal, compare]

and value =
  | Constant of word
  | Storage of {
      at_entry: string option;
      at_exit : string option;
    }
  | Memory of memory
[@@deriving equal, compare]

let name t = t.name
let value t = t.value

let constant (v : value) = match v with
  | Constant w -> Some w
  | _ -> None

let at_entry (v : value) = match v with
  | Storage {at_entry; _} -> at_entry
  | _ -> None
    
let at_exit (v : value) = match v with
  | Storage {at_exit; _} -> at_exit
  | _ -> None

let memory (v : value) = match v with
  | Memory m -> Some m
  | _ -> None

let frame (m : memory) = match m with
  | Frame (base, off) -> Some (base, off)
  | _ -> None

let global (m : memory) = match m with
  | Global addr -> Some addr
  | _ -> None

let create_frame (fp : string) (off : word) : memory =
  Frame (fp, off)

let create_global (addr : word) : memory =
  Global addr

let create_with_memory (name : string) ~(memory : memory) : t =
  {name; value = Memory memory}

let create_with_constant (name : string) ~(const : word) : t =
  {name; value = Constant const}

let create_with_storage
    (name : string)
    ~(at_entry : string option)
    ~(at_exit : string option) : t =
  {name; value = Storage {at_entry; at_exit}}

let is_reg (v : value) : bool =
  match v with
  | Storage _ -> true
  | _ -> false

let is_mem (v : value) : bool =
  match v with
  | Memory _ -> true
  | _ -> false

let is_frame (v : memory) : bool =
  match v with
  | Frame _ -> true
  | _ -> false

let is_global (v : memory) : bool =
  match v with
  | Global _ -> true
  | _ -> false

let is_constant (v : value) : bool =
  match v with
  | Constant _ -> true
  | _ -> false

let is_storage (v : value) : bool =
  match v with
  | Storage _ -> true
  | _ -> false

let find (name : string) (vars : t list) : t option =
  List.find vars ~f:(fun t -> String.equal name t.name)
