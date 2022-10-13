open Core
open Bap.Std
open Bap_core_theory

module T = Theory
module CT = Vibes_utils.Core_theory
module Sexps = Vibes_utils.Sexp
module Files = Vibes_utils.Files
module Log = Vibes_log.Stream
module Inputs = Vibes_constants.Inputs

let (let*) x f = Result.bind x ~f

let run
    ~(target : string)
    ~(language : string)
    ~(patch_names : string list)
    ~(model_filepath : string)
    ~(binary : string)
    ~(patched_binary : string)
    () : (unit, KB.conflict) result =
  Log.send "Vibes_init.Runner.run '%s' '%s' '%s' '%s' '%s' '%s'"
    target language (String.concat patch_names ~sep:", ")
    model_filepath binary patched_binary;
  let* target = CT.get_target target in    
  let* language = CT.get_language language in
  let t = Types.create target language ~patch_names
      ~model:model_filepath ~binary ~patched_binary
      ~spaces:Inputs.default_patch_spaces in
  Log.send "Generating template files";
  let* () = Types.generate_files t in
  Log.send "Generating Makefile";
  let data = Format.asprintf "%a" Types.pp_makefile t in
  let* () = Files.write_or_error data "Makefile" in
  Ok ()
