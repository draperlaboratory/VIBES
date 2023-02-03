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

open Bap.Std
open Bap_core_theory

(** JSON representation of [Bap.Std.Bitvector]. *)
module Bitvector : sig

  type t = word [@@deriving compare, equal, sexp]

  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t

end

(** JSON representation of tids ([Theory.label]s). *)
module Label : sig

  type t = string [@@deriving compare, equal, sexp]

  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  
end

(** Pretty-prints the data in JSON format. *)
val pp :
  yojson_of_t:('a -> Yojson.Safe.t) ->
  Format.formatter ->
  'a ->
  unit

(** [from_file ~yojson_of_t ~t_of_yojson filename] attempts to
    deserialize the contents of the file [filename] according to
    [t_of_yojson]. *)
val from_file :
  yojson_of_t:('a -> Yojson.Safe.t) ->
  t_of_yojson:(Yojson.Safe.t -> 'a) ->
  string ->
  ('a, KB.conflict) result
