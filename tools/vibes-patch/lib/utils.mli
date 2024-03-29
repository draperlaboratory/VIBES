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

(** A region in the binary with a file offset. *)
type region = {
  addr   : int64;
  size   : int64;
  offset : int64;
}

(** A region in the binary with a name. *)
type named_region = {
  addr : int64;
  size : int64;
  name : string;
}

(** A chunk of code associated with a named symbol. *)
type symbol_chunk = {
  addr : int64;
  size : int64;
  root : int64;
}

(** [find_code_region addr spec] looks up the code region in [spec] that
    contains [addr]. *)
val find_code_region : int64 -> Ogre.doc -> region option

(** [find_mapped_region addr spec] looks up the mapped region in [spec] that
    contains [addr]. *)
val find_mapped_region : int64 -> Ogre.doc -> region option

(** [find_named_region addr spec] looks up the named region in [spec] that
    contains [addr]. *)
val find_named_region : int64 -> Ogre.doc -> named_region option

(** [find_symbol_chunk addr spec] looks up the symbol chunk in [spec] that
    contains [addr]. *)
val find_symbol_chunk : int64 -> Ogre.doc -> symbol_chunk option

(** [find_named_symbol addr spec] looks up the named symbol in [spec] that
    has the address [addr]. *)
val find_named_symbol : int64 -> Ogre.doc -> string option

(** Converts a virtual address into a file offset. Assumes that the address
    is contained within the region. *)
val addr_to_offset : int64 -> region -> int64

(** Converts a file offset into a virtual address. Assumes that the offset
    is contained within the region. *)
val offset_to_addr : int64 -> region -> int64
