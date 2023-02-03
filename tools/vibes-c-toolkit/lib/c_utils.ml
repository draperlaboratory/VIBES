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

let print_c (pp : 'a -> unit) (data : 'a) : string =
  let tmp_file, chan = Stdlib.Filename.open_temp_file "frontc" ".out" in
  let old_chan = !Cprint.out in
  Cprint.out := chan;
  pp data;
  Cprint.flush ();
  Out_channel.close chan;
  let res = In_channel.read_lines tmp_file |> String.concat ~sep:"\n" in
  Cprint.out := old_chan;
  Sys_unix.remove tmp_file;
  res
