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

open Core
open Bap.Std
open Bap_core_theory

module Log = Vibes_log.Stream
module Utils = Vibes_utils
module Hvar = Vibes_higher_vars.Higher_var

type space = {
  address : Utils.Json.Bitvector.t [@key "address"];
  size : int64 [@key "size"];
} [@@deriving yojson]

type spaces = space list [@@deriving yojson]

module Spaces = struct

  module Tree = Interval_tree.Make(struct
      type t = addr * addr [@@deriving sexp]
      type point = addr [@@deriving sexp]

      let compare_point : point -> point -> int = Addr.compare

      let compare ((x1, y1) : t) ((x2, y2) : t) : int =
        match compare_point x1 x2 with
        | 0 -> compare_point y1 y2
        | n -> n

      let lower : t -> point = fst
      let upper : t -> point = snd
    end)

  type t = unit Tree.t

  let empty : t = Tree.empty
  let is_empty : t -> bool = Tree.is_empty

  let of_list (l : space list) : t =
    (* Get an initial widening of spaces that start on the
       same address. We also convert the size into an end
       address (being the upper bound on the region). *)
    List.fold l ~init:Addr.Map.empty ~f:(fun m {address; size} ->
        if Int64.(size > 0L) then
          let width = Addr.bitwidth address in
          let size = Addr.of_int64 size ~width in
          let end_ = Addr.(pred (address + size)) in
          Map.update m address ~f:(function
              | Some e when Addr.(end_ < e) -> e
              | Some _ | None -> end_)
        else m) |> Map.to_sequence |>
    Seq.fold ~init:empty ~f:(fun t i ->
        (* For all intervals that intersect with this one, we widen the
           current interval until it subsumes all of them. *)
        Tree.intersections t i |> Seq.map ~f:fst |>
        Seq.fold ~init:(t, i) ~f:(fun (t, (x1, y1)) ((x2, y2) as i) ->
            Tree.remove t i, Addr.(min x1 x2, max y1 y2)) |> fun (t, i) ->
        Tree.add t i ())

  let to_list (t : t) : space list =
    Tree.to_sequence t |>
    Seq.map ~f:(fun ((start, end_), ()) ->
        let size =
          Bitvec.to_int64 @@
          Addr.to_bitvec @@
          Addr.succ @@
          Addr.(end_ - start) in
        {address = start; size}) |>
    Seq.to_list

  let yojson_of_t (t : t) : Yojson.Safe.t =
    to_list t |> yojson_of_spaces

  let t_of_yojson (j : Yojson.Safe.t) : t =
    spaces_of_yojson j |> of_list

  let pp : Format.formatter -> t -> unit =
    Utils.Json.pp ~yojson_of_t

  let from_file : string -> (t, KB.conflict) result =
    Utils.Json.from_file ~yojson_of_t ~t_of_yojson

end

type t = {
  patch_point : Utils.Json.Bitvector.t [@key "patch-point"];
  patch_size : int64 [@key "patch-size"];
  sp_align : int [@key "sp-align"];
  patch_vars : Hvar.t list [@default []] [@key "patch-vars"];
} [@@deriving yojson]

let pp : Format.formatter -> t -> unit = Utils.Json.pp ~yojson_of_t

let from_file (filename : string) : (t, KB.conflict) result =
  let (let*) x f = Result.bind x ~f in
  let* info = Utils.Json.from_file filename ~yojson_of_t ~t_of_yojson in
  if Int64.(info.patch_size < 0L) then
    let msg = Format.sprintf "Bad `patch-size` value %Ld" info.patch_size in
    Error (Errors.Json_parse_error msg)
  else Ok info
