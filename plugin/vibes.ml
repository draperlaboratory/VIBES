(* The VIBES front-end (a CLI).

   Currently, the front end is a BAP custom command. *)

open !Core_kernel
open Bap_main
open Bap.Std
open Bap_knowledge

module KB = Knowledge
module Cmd = Extension.Command
module Typ = Extension.Type

type Extension.Error.t += Fail of string

(* A module to encapsulate the definition of the CLI. *)
module Cli = struct

  (* Name and description of the custom command. *)
  let name = "vibes"
  let doc = "Vibes is a CEGIS-driven binary patcher."

  (* --config=FILEPATH  e.g.,  --config=patch.json
     Specify the patch configuration file.
   *)
  let config_filepath = Cmd.parameter Typ.path "config"
    ~doc:"Patch configuration file"

  (* [--output,-o]=FILEPATH  e.g., --output=main.patched
     Specify the output filepath for the patched binary. *)
  let patched_exe_filepath = Cmd.parameter (Typ.some Typ.path) "output"
    ~doc:"The output location for the patched binary (a filename)"
    ~aliases:["o"]

  (* --verbose
     If present, turn on verbose logging. *)
  let verbose = Cmd.flag "verbose"
    ~doc:"Print verbose output?"

  (* --no-colors
     If present, print verbose log with no colors. *)
  let no_colors = Cmd.flag "no-colors"
    ~doc:"Print verbose output without colors?"

  (* EXE, e.g., /path/to/pre-patched/exe
     This is the only positional argument of the CLI. It should be the path
     to the original executable that is to be patched. *)
  let exe = Cmd.argument Typ.file
    ~doc:"Path to the binary you want to patch"

  (* The grammar of the CLI. *)
  let grammar = Cmd.(args
    $ config_filepath $ patched_exe_filepath $ verbose $ no_colors $ exe)

  (* The callback that BAP invokes when you call [bap vibes] from
     the command line. *)
  let callback (config_filepath : string)
        (patched_exe_filepath : string option)
        (verbose : bool) (no_colors : bool)
        (exe : string) (ctxt : ctxt)
      : (unit, error) result =

    (* If the user requested verbose logging, subscribe the verbose logger 
       to the event stream, so it can report those events. *)
    let () = match verbose with
      | true ->
        begin
          let module Config : Bap_vibes.Verbose.Config = struct
            let with_colors = not no_colors
          end in
          let module Verbose_log = Bap_vibes.Verbose.Stderr (Config) in
          Bap_vibes.Events.subscribe Verbose_log.handle
        end
      | false -> ()
    in

    (* Report startup *)
    Bap_vibes.Events.(send @@ Header "Starting VIBES");
    Bap_vibes.Events.(send @@ Info (Printf.sprintf "Verbose: %b" verbose));
    let () = match verbose with
      | true ->
        begin
          let msg = Printf.sprintf "With colors: %b" (not no_colors) in
          Bap_vibes.Events.(send @@ Info msg)
        end
      | false -> ()
    in

    (* Parse the command line arguments. *)
    let result = Vibes_plugin_parameters.create
        ~exe ~config_filepath ~patched_exe_filepath
    in
    match result with
    | Error e -> Error (Fail (Format.asprintf "%a" Vibes_plugin_errors.pp e))
    | Ok config ->
      begin

        (* Run the pipeline and print the result. If success, the result is
           the filepath to the patched executable. Otherwise, error. *)
        let result = Bap_vibes.Pipeline.run config in
        match result with
        | Ok filepath ->
          begin
            print_endline filepath;
            Ok ()
          end
        | Error e ->
          Error (Fail (Format.asprintf "%a" Bap_vibes.Toplevel_error.pp e))

      end

end

(* Register the custom CLI/command with BAP. *)
let () = Cmd.declare Cli.name Cli.grammar Cli.callback ~doc:Cli.doc
