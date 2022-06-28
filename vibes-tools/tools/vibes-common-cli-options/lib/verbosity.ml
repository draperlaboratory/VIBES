module C = Cmdliner
module Log = Vibes_log_lib.Stream

let start_color = "\x1b[33m"
let end_color = "\x1b[0m"

let tty_reporter (event : Log.event) : unit =
  Format.eprintf "%s[Verbose.log] %s%s\n%!" start_color event end_color

let no_tty_reporter (event : Log.event) : unit =
  Format.eprintf "[Verbose.log] %s\n%!" event

let setup (is_verbose : bool) (is_no_color : bool) : unit =
  match is_verbose with
  | true ->
    if is_no_color then Log.subscribe no_tty_reporter
    else Log.subscribe tty_reporter
  | false -> ()

let is_verbose : bool C.Term.t =
  let info = C.Arg.info ["v"; "verbose"]
    ~docv:"VERBOSE"
    ~doc:"Display verbose output?"
  in
  C.Arg.value (C.Arg.flag info)

let is_no_color : bool C.Term.t =
  let info = C.Arg.info ["no-color"]
    ~docv:"NO-COLOR"
    ~doc:"Suppress color output?"
  in
  C.Arg.value (C.Arg.flag info)
