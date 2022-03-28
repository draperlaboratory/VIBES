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

(** Encapsulates higher variables for patches.

    A higher variable is a variable defined in a higher language,
    like in C, or in decompiled C-like code. Such higher variables
    have names (e.g., "x" or "my_counter"), and at the entry and
    exit of a patch, they are stored either in a register, or on
    the stack at some offset past the framepointer.
*)

open !Core_kernel
open Bap.Std

(** A memory location can be a frame pointer register with an offset,
    or an absolute address which points to global memory. *)
type memory =
  | Frame of string * word
  | Global of word
[@@deriving equal, compare]

(** A higher variable has a name and a value. *)
type t = {
  name : string;
  value : value;
} [@@deriving equal, compare]

(** A value can be a constant, a set of registers, or a memory location. *)
and value =
  | Constant of word
  | Storage of {
      at_entry: string option;
      at_exit : string option;
    }
  | Memory of memory
[@@deriving equal, compare]

(** Accessors. *)
val name : t -> string
val value : t -> value
val constant : value -> word option
val at_entry : value -> string option
val at_exit : value -> string option
val memory : value -> memory option
val frame : memory -> (string * word) option
val global : memory -> word option

(** [create_frame fp off] creates a memory location in a stack frame. *)
val create_frame : string -> word -> memory

(** [create_global addr] creates a location in global memory. *)
val create_global : word -> memory

(** [create_with_constant name ~const] creates a new higher variable that
    aliases a constant value. *)
val create_with_constant : string -> const:word -> t

(** [create_with_storage name ~at_entry ~at_exit] creates a new higher
    variable that aliases a register. *)
val create_with_storage :
  string -> at_entry:string option -> at_exit:string option -> t

(** [create_with_memory name ~memory] creates a new higher var that aliases
    a memory location. *)
val create_with_memory : string -> memory:memory -> t

(** [equal t1 t2] checks if higher var records [t1] and [t2] are equal. *)
val equal : t -> t -> bool

(** Is the variable stored in a register? *)
val is_reg : value -> bool

(** Is the variable stored in memory? *)
val is_mem : value -> bool

(** Is the variable stored in a stack frame? *)
val is_frame : memory -> bool

(** Is the variable stored in global memory? *)
val is_global : memory -> bool

(** Is the variable aliasing a constant value? *)
val is_constant : value -> bool

(** [find name h_vars] finds the record for a higher variable named [name]. *)
val find : string -> t list -> t option
