(** Implements {!Higher_var}. *)

open Core
open Bap.Std

module Utils = Vibes_utils_lib

(* For a [Frame], we have the name of the frame pointer register
   and an offset. *)
type memory =
  | Frame of string * Utils.Json.Bitvector.t [@name "frame"]
  | Global of Utils.Json.Bitvector.t [@name "address"]
[@@deriving yojson, equal, compare]

type value =
  | Constant of Utils.Json.Bitvector.t [@name "constant"]
  | Registers of {
      at_entry: string option [@yojson.option] [@key "at-entry"];
      at_exit : string option [@yojson.option] [@key "at-exit"];
    } [@name "register"]
  | Memory of memory [@name "memory"]
[@@deriving yojson, equal, compare]

type t = {
  name : string;
  value : value [@key "storage-class"];
} [@@deriving yojson, equal, compare]

let name t = t.name
let value t = t.value

let constant (v : value) = match v with
  | Constant w -> Some w
  | _ -> None

let at_entry (v : value) = match v with
  | Registers {at_entry; _} -> at_entry
  | _ -> None
    
let at_exit (v : value) = match v with
  | Registers {at_exit; _} -> at_exit
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

let create_with_registers
    (name : string)
    ~(at_entry : string option)
    ~(at_exit : string option) : t =
  {name; value = Registers {at_entry; at_exit}}

let is_reg (v : value) : bool =
  match v with
  | Registers _ -> true
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
  | Registers _ -> true
  | _ -> false

let find (name : string) (vars : t list) : t option =
  List.find vars ~f:(fun t -> String.equal name t.name)
