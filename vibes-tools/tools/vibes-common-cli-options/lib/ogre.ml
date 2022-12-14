module C = Cmdliner

let ogre : string option C.Term.t =
  let info = C.Arg.info ["O"; "ogre"]
      ~docv:"OGRE"
      ~doc:"Optional path/name of OGRE file for the binary" in
  let parser = C.Arg.some' C.Arg.string in
  let default = None in
  let arg = C.Arg.opt parser default info in
  C.Arg.value arg
