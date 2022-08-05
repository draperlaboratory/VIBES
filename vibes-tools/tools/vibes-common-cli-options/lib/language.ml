module C = Cmdliner

let language : string C.Term.t =
  let info = C.Arg.info ["l"; "language"]
    ~docv:"LANGUAGE"
    ~doc:"Name of language (e.g., \"llvm-armv7\", etc.)" in
  let parser = C.Arg.some' C.Arg.string in
  let default = None in
  let arg = C.Arg.opt parser default info in
  C.Arg.required arg
