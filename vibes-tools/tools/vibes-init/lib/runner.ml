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

let collect_ogre (binary : string) : (string, KB.conflict) result =
  Log.send "Generating OGRE file %s for binary %s" Inputs.default_ogre binary;
  let* image = Vibes_patch.Loader.image binary in
  let data = Format.asprintf "%a" Ogre.Doc.pp @@ Image.spec image in
  let* () = Files.write_or_error data Inputs.default_ogre in
  Ok Inputs.default_ogre

let run
    ?(ogre_filepath : string option = None)
    ~(target : string)
    ~(language : string)
    ~(patch_names : string list)
    ~(model_filepath : string)
    ~(binary : string)
    ~(patched_binary : string)
    () : (unit, KB.conflict) result =
  Log.send "Vibes_init.Runner.run '%s' '%s' '%s' '%s' '%s' '%s' '%s'"
    target language (String.concat patch_names ~sep:", ")
    model_filepath binary patched_binary
    (Option.value ogre_filepath ~default:Inputs.default_ogre);
  let* target = CT.get_target target in    
  let* language = CT.get_language language in
  let* ogre = match ogre_filepath with
    | None -> collect_ogre binary
    | Some ogre -> Ok ogre in
  let t =
    Types.create target language ~patch_names
      ~model:model_filepath ~binary ~patched_binary
      ~ogre ~spaces:Inputs.default_patch_spaces in
  Log.send "Generating template files";
  let* () = Types.generate_files t in
  Log.send "Generating Makefile";
  let data = Format.asprintf "%a" Types.pp_makefile t in
  let* () = Files.write_or_error data "Makefile" in
  Ok ()
