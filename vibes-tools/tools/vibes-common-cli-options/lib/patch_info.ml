module C = Cmdliner

let filepath : string C.Term.t =
  let info = C.Arg.info ["p"; "patch-info-filepath"]
      ~docv:"PATCH_INFO_FILEPATH"
      ~doc:"Filepath containing patch info (JSON)" in
  let parser = C.Arg.some' C.Arg.string in
  let default = None in
  let arg = C.Arg.opt parser default info in
  C.Arg.required arg

let spaces : string option C.Term.t =
  let info = C.Arg.info ["patch-spaces"]
      ~docv:"PATCH_SPACES"
      ~doc:"Filepath containing external patch spaces (JSON)" in
  let parser = C.Arg.some' C.Arg.string in
  let default = None in
  let arg = C.Arg.opt parser default info in
  C.Arg.value arg
