(* Implements {!Exe_ingester}. *)

open !Core_kernel
open Bap.Std
open Bap_knowledge
open Knowledge.Syntax

module KB = Knowledge

(* A type for the loader that [ingest] uses below. *)
type loader = string -> Project.t KB.t

(* Get the address size for a [Project.t]. *)
let addr_size_of (proj : Project.t) : int =
  let arch = Project.arch proj in
  match Arch.addr_size arch with
  | `r32 -> 32
  | `r64 -> 64

(* Pull in the original exe. Right now, we do nothing with it, except
   get the address size and stash that in the KB for others to use. *)
let ingest ?loader:(loader=Exe_loader.load) (obj : Data.t) : unit KB.t =
  Events.(send @@ Header "Starting exe ingester");

  (* Get the filepath to the exe to load. *)
  Events.(send @@ Info "Retreiving data from KB...");
  Data.Original_exe.get_filepath_exn obj >>= fun filepath ->

  Events.(send @@ Info (Format.sprintf "Loading into BAP: %s..." filepath));

  (* Load the project and stash the program in the KB. *)
  loader filepath >>= fun project ->
  let program = Project.program project in
  Data.Original_exe.set_prog obj (Some program) >>= fun _ ->

  (* Get the address size of the project and stash it in the KB. *)
  let addr_size = addr_size_of project in
  Data.Original_exe.set_addr_size obj (Some addr_size) >>= fun _ ->
  Events.(send @@ Info (Printf.sprintf "Address size: %d bits" addr_size));

  KB.return ()
