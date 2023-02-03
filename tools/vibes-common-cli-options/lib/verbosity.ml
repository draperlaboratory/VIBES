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
module Log = Vibes_log.Stream

let start_color : string = "\x1b[33m"
let end_color : string = "\x1b[0m"

let tty_reporter (event : Log.event) : unit =
  Format.eprintf "%s[Verbose.log] %s%s\n%!" start_color event end_color

let no_tty_reporter (event : Log.event) : unit =
  Format.eprintf "[Verbose.log] %s\n%!" event

let setup ~(verbose : bool) ~(no_color : bool) : unit =
  if verbose then
    if no_color then Log.subscribe no_tty_reporter
    else Log.subscribe tty_reporter

let verbose : bool C.Term.t =
  let info = C.Arg.info ["v"; "verbose"]
    ~docv:"VERBOSE"
    ~doc:"Display verbose output?" in
  C.Arg.value (C.Arg.flag info)

let no_color : bool C.Term.t =
  let info = C.Arg.info ["no-color"]
    ~docv:"NO-COLOR"
    ~doc:"Suppress color output?" in
  C.Arg.value (C.Arg.flag info)
