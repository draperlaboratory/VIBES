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

  (* --patch=NAME, e.g., --patch=ret-3
     Specify the NAME of the hand-written patch to use. *)
  let patch_doc = Printf.sprintf 
    "Name of predefined patch to select. Options: %s"
    (String.concat ~sep:", " Patches.names)
  let patch = Cmd.parameter Typ.string "patch"
    ~doc:patch_doc

  (* --patch-point=HEX, e.g., --patch-point=0x54
     Specify the address in the executable to start patching at. *)
  let patch_point = Cmd.parameter Typ.string "patch-point"
    ~doc:"Address (in hex) at which to insert patch, e.g. 0x3fffffe0"

  (* --patch-size=INT, e.g., --patch-size=8
     Specify the number of bytes to overwrite with the patch. *)
  let patch_size = Cmd.parameter Typ.int "patch-size"
    ~doc:"Size of patch (number of bytes to replace), e.g. 16"

  (* --property=SEXP, e.g., --property="(true)"
     Specify the correctness property (as S-expression) that should
     be used to verify the patched executable. *) 
  let property = Cmd.parameter Typ.string "property"
    ~doc:"A correctness property (as an S-expression)"

  (* [--output,-o]=FILEPATH  e.g., --output=main.patched
     Specify the output filepath for the patched binary. *)
  let patched_exe_filepath = Cmd.parameter (Typ.some Typ.path) "output"
    ~doc:"The output location for the patched binary (a filename)"
    ~aliases:["o"]

  (* --max-tries=INT, e.g., --max-tries=10
     Specify the number of CEGIS iterations to allow before giving up. *)
  let max_tries = Cmd.parameter (Typ.some Typ.int) "max-tries"
    ~doc:"Number of CEGIS iterations to allow before giving up." 

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
    $ patch $ patch_point $ patch_size $ property $ patched_exe_filepath
    $ max_tries $ verbose $ no_colors $ exe)

  (* The callback that BAP invokes when you call [bap vibes] from
     the command line. *)
  let callback (patch : string) (patch_point : string) (patch_size : int)
        (property : string) (patched_exe_filepath : string option)
        (max_tries : int option) (verbose : bool) (no_colors : bool)
        (exe : string) (ctxt : ctxt)
      : (unit, error) result =

    (* If the user requested verbose logging, subscribe the verbose logger 
       to the event stream, so it can report those events. *)
    let () = match verbose with
      | true ->
        begin
          let module Config : Verbose.Config = struct
            let with_colors = not no_colors
          end in
          let module Verbose_log = Verbose.Stderr (Config) in
          Events.subscribe Verbose_log.handle
        end
      | false -> ()
    in

    (* Report startup *)
    Events.(send @@ Header "Starting VIBES");
    Events.(send @@ Info (Printf.sprintf "Verbose logging: %b" verbose));
    let () = match verbose with
      | true ->
        begin
          let msg = Printf.sprintf "With colors: %b" (not no_colors) in
          Events.(send @@ Info msg)
        end
      | false -> ()
    in

    (* Parse the command line arguments. *)
    let result =
      Config.create ~exe ~patch ~patch_point ~patch_size ~property
        ~patched_exe_filepath ~max_tries
    in
    match result with
    | Error e -> Error (Fail (Format.asprintf "%a" Config.Errors.pp e)) 
    | Ok config ->
      begin

        (* Run the pipeline and print the result. If success, the result is
           the filepath to the patched executable. Otherwise, error. *)
        let result = Pipeline.run config in
        match result with
        | Ok filepath ->
          begin
            print_endline filepath;
            Ok ()
          end
        | Error e -> Error (Fail (Format.asprintf "%a" KB.Conflict.pp e))

      end

end

(* Register the custom CLI/command with BAP. *)
let () = Cmd.declare Cli.name Cli.grammar Cli.callback ~doc:Cli.doc
