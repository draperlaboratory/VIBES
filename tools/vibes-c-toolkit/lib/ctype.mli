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

open Bap_c.Std

(** Lookup for named types. *)
type gamma = string -> C.Type.t

(** Lookup for structures and unions. *)
type tag = {
  lookup : 'a. (string -> 'a list -> C.Type.t) -> string -> C.Type.t;
}

(** Converts a FrontC type to a BAP C type. *)
val ctype : gamma -> tag -> Cabs.base_type -> C.Type.t

(** Conversion back to the FrontC representation. *)
module To_cabs : sig

  (** Convert a BAP C type to a FrontC type. *)
  val go : C.Type.t -> Cabs.base_type

end
