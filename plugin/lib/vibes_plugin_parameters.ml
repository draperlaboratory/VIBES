(* Implements {!Vibes_plugin_parameters}. *)

open !Core_kernel
open Monads.Std

module Json = Yojson.Safe
module Vibes_config = Bap_vibes.Config
module Errors = Vibes_plugin_errors

(* Monadize the errors. *)
module Err = Monad.Result.Make (Errors) (Monad.Ident)
open Err.Syntax
type error = Errors.t Err.error

(* Error if a string is empty. *)
let is_not_empty (value : string) (e : Errors.t)
    : (string, error) Stdlib.result =
  match String.length value with
  | 0 -> Err.fail e
  | _ -> Err.return value

(* Parse the user-provided JSON config file into a Yojson.Safe.t *)
let parse_json (config_filepath : string) : (Json.t, error) Stdlib.result =
  try
    Err.return (Json.from_file config_filepath)
  with e -> Err.fail (Errors.Config_not_parsed (Exn.to_string e))

(* Construct a configuration record from the given parameters. *)
let create ~exe:(exe : string) ~config_filepath:(config_filepath : string)
      ~patched_exe_filepath:(patched_exe_filepath : string option)
    : (Vibes_config.t, error) result =
  is_not_empty exe Errors.Missing_exe >>= fun exe ->
  parse_json config_filepath >>= fun config_json ->
  match Vibes_config.t_of_yojson ~exe ~patched_exe_filepath config_json with
    | Ok config -> Err.return config
    | Error msg -> Err.fail (Errors.Config_not_parsed msg)
