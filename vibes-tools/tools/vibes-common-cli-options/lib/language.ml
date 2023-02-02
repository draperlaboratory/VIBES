module C = Cmdliner

let language : string C.Term.t =
  let info = C.Arg.info ["l"; "language"]
    ~docv:"LANGUAGE"
    ~doc:"Name of language (e.g., \"llvm-armv7\", \
          \"llvm-thumb\", etc.)" in
  let parser = C.Arg.some' C.Arg.string in
  let default = None in
  let arg = C.Arg.opt parser default info in
  C.Arg.required arg

let language_optional : string option C.Term.t =
  let info = C.Arg.info ["l"; "language"]
    ~docv:"LANGUAGE"
    ~doc:"Optional name of language (e.g., \
          \"llvm-armv7\", \"llvm-thumb\", etc.). \
          If left unspecified, VIBES will attempt to \
          infer this from the binary using some heuristics \
          (they are not guaranteed to be sound)." in
  let parser = C.Arg.some' C.Arg.string in
  let default = None in
  let arg = C.Arg.opt parser default info in
  C.Arg.value arg
