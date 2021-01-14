(* Implements {!Patch_ingester}. *)

open Bap.Std
open Bap_knowledge
open Knowledge.Syntax

module KB = Knowledge

(* Loads the BIR version of a patch. For now, we select from a hand-written
   set of patches defined in the {!Patches} module. *)
let ingest (obj : Data.t) : unit KB.t =
  Events.(send @@ Header "Starting patch ingester");
  Events.(send @@ Info "Using hand-written BIR patches");

  (* Get the name of the patch we want to load. *)
  Events.(send @@ Info "Retreiving data from KB...");
  Data.Patch.get_patch_name_exn obj >>= fun name ->
  Events.(send @@ Info (Printf.sprintf "Selecting patch named: %s" name));

  (* Get the patch (as BIR). *)
  Data.Original_exe.get_addr_size_exn obj >>= fun addr_size ->
  Patches.get_bir name addr_size >>= fun bir ->

  (* Stash the BIR in the KB. *)
  Data.Patch.set_bir obj bir >>= fun _ ->

  Events.(send @@ Info "Done. The patch has the following BIL:");
  Events.(send @@ Rule);
  let bir_str = Format.asprintf "%a" Bil.pp (KB.Value.get Bil.slot bir) in
  Events.(send @@ Info bir_str);
  Events.(send @@ Rule);

  KB.return ()
