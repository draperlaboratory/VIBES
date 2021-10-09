(***********************************************************************
*
*   This module provides a custom "lightweight" loader for binaries,
*   configured by user-input.
*
*   An almost verbatim copy of the "raw" loader for BAP: this allows us
*   fine-grained control of the loading process, effectively allowing
*   us to disassemble only small chunks of a large binary.
*
*
**********************************************************************)


let _doc = {|
# DESCRIPTION

Provides a VIBES specific loader for raw binaries. Raw binaries do not contain any
meta information or other headers, so this input should be provided
form the outside.

|}

open Bap.Std
open Core_kernel

let register_loader conf =
  match Config.ogre conf with
  | None -> false
  | Some o ->
    begin
      Image.register_loader ~name:"vibes-raw" (module struct
        let from_file _ = Or_error.map ~f:Option.some (Ogre.Doc.from_string o)
        let from_data _ = Or_error.map ~f:Option.some (Ogre.Doc.from_string o)
      end);
      true
    end

