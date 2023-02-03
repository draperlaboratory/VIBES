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

let dedup_list_stable l ~compare =
  let equal x x' = compare x x' = 0 in
  let rec loop res = function
    | [] -> res
    | x :: xs ->
      let dups = List.find_all_dups (x :: xs) ~compare in
      let res = if List.mem dups x ~equal then res else x :: res in
      loop res xs in
  loop [] (List.rev l)
