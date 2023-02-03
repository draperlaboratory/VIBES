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

module C = Cmdliner

let target : string C.Term.t =
  let info = C.Arg.info ["t"; "target"]
    ~docv:"TARGET"
    ~doc:"Name of target (see 'bap list targets')" in
  let parser = C.Arg.some' C.Arg.string in
  let default = None in
  let arg = C.Arg.opt parser default info in
  C.Arg.required arg
