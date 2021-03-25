(* Implements {!Exe_info}. *)

open !Core_kernel
open Bap.Std
open Bap_knowledge
open Knowledge.Syntax
open Bap_core_theory
module KB = Knowledge

(* Extract info about an executable and stash it in the KB. *)
let extract (obj : Data.t) (proj : Project.t) : unit KB.t =
  Events.(send @@ Header "Starting exe ingester");

  (* Get the address size of the project and stash it in the KB. *)
  let target = Project.target proj in
  let addr_size = Theory.Target.bits target in
  Data.Original_exe.set_addr_size obj (Some addr_size) >>= fun _ ->
  Events.(send @@ Info (Printf.sprintf "Address size: %d bits" addr_size));

  KB.return ()
