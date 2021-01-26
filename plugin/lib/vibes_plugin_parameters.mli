(* Handles parsing of plugin parameters (provided at the command line). 

   The primary goal is to set up a [Bap_vibes.Config.t] configuration record,
   which is needed to run the [Bap_vibes] pipeline.

   The configuration data we use for that is collected from a JSON config file.
   Appropriate errors are returned if there are errors in the JSON. *)

open !Core_kernel
open Monads.Std

module Errors = Vibes_plugin_errors

(* Error results are derived from the {!Errors} module. *)
type error = Errors.t Monad.Result.Make (Errors) (Monad.Ident).error

(* [create ~exe ~config_filepath ~patched_exe_filepath] will create
   a [Bap_vibes.Config.t] configuration record from a JSON config file, where:
   - [~exe] is the filepath to the original executable
   - [~config_filepath] is the filepath to the JSON config file
   - [~patched_exe_filepath] is the optional output location *)
val create : exe:string -> config_filepath:string ->
  patched_exe_filepath:string option -> (Bap_vibes.Config.t, error) result
