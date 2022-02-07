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

(** A higher variable is stored either in a register,
    or at some memory location. *)
type stored_in =
  | Register of string
  | Memory of memory
[@@deriving equal, compare]

(** A memory location can be a frame pointer register with an offset,
    or an absolute address which points to global memory. *)
and memory =
  | Frame of string * word
  | Global of word
[@@deriving equal, compare]

(** A higher variable has a name and a value. *)
type t = {
  name : string;
  value : value;
} [@@deriving equal, compare]

(** A value can be a constant, or it can have some storage classification. *)
and value =
  | Constant of word
  | Storage of {
      at_entry: stored_in;
      at_exit : stored_in option;
    }
[@@deriving equal, compare]

(** Accessors. *)
val name : t -> string
val value : t -> value
val constant : value -> word option
val at_entry : value -> stored_in option
val at_exit : value -> stored_in option
val register : stored_in -> string option
val memory : stored_in -> memory option
val frame : memory -> (string * word) option
val global : memory -> word option

(** [stored_in_register reg] creates a register storage classifier. *)
val stored_in_register : string -> stored_in

(** [create_frame fp off] creates a memory location in a stack frame. *)
val create_frame : string -> word -> memory

(** [create_global addr] creates a location in global memory. *)
val create_global : word -> memory

(** [stored_in_memory mem] creates a memory storage classifier. *)
val stored_in_memory : memory -> stored_in

(** [create_with_constant name ~const] creates a new higher variable that
    aliases a constant value. *)
val create_with_constant : string -> const:word -> t

(** [create_with_storage name ~at_entry ~at_exit] creates a new higher variable
    that aliases either a register or a location in memory. *)
val create_with_storage :
  string -> at_entry:stored_in -> at_exit:(stored_in option) -> t

(** [equal t1 t2] checks if higher var records [t1] and [t2] are equal. *)
val equal : t -> t -> bool

(** Is the variable stored in a register? *)
val is_reg : stored_in -> bool

(** Is the variable stored in memory? *)
val is_mem : stored_in -> bool

(** Is the variable stored in a stack frame? *)
val is_frame : memory -> bool

(** Is the variable stored in global memory? *)
val is_global : memory -> bool

(** Is the variable aliasing a constant value? *)
val is_constant : value -> bool

(** Does the variable have a storage classification? *)
val is_storage : value -> bool

(** [find name h_vars] finds the record for a higher variable named [name]. *)
val find : string -> t list -> t option
