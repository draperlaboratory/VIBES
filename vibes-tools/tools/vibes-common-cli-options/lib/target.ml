module C = Cmdliner

let target : string C.Term.t =
  let info = C.Arg.info ["t"; "target"]
    ~docv:"TARGET"
    ~doc:"Name of target (see 'bap list targets')"
  in
  let parser = C.Arg.some' C.Arg.string in
  let default = None in
  let arg = C.Arg.opt parser default info in
  C.Arg.required arg
