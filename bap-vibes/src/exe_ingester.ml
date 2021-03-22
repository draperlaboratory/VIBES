(* Implements {!Exe_ingester}. *)

open !Core_kernel
open Bap.Std
open Bap_knowledge
open Knowledge.Syntax
open Bap_core_theory
module KB = Knowledge

(* Pull in the original exe. Right now, we do nothing with it, except
   get the address size and stash that in the KB for others to use. *)
let ingest (obj : Data.t) (proj : Project.t) : unit KB.t =
  Events.(send @@ Header "Starting exe ingester");

  (* Get the filepath to the exe to load. *)
  Events.(send @@ Info "Retreiving data from KB...");

  (* Get the address size of the project and stash it in the KB. *)
  let target = Project.target proj in
  let addr_size = Theory.Target.bits target in
  Data.Original_exe.set_addr_size obj (Some addr_size) >>= fun _ ->
  Events.(send @@ Info (Printf.sprintf "Address size: %d bits" addr_size));

  KB.return ()
