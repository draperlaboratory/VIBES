(* Implements {!Patch_ingester}. *)

open Bap.Std
open Bap_knowledge
open Knowledge.Syntax

module KB = Knowledge 

(* Loads the BIL version of a patch. For now, we select from a hand-written
   set of patches defined in the {!Patches} module. *)
let ingest (obj : Data.t) : unit KB.t =
  Events.(send @@ Header "Starting patch ingester");
  Events.(send @@ Info "Using hand-written BIL patches");

  (* Get the name of the patch we want to load. *)
  Events.(send @@ Info "Retreiving data from KB...");
  Data.Patch.get_patch_name_exn obj >>= fun name ->
  Events.(send @@ Info (Printf.sprintf "Selecting patch named: %s" name));

  (* Get the patch (as BIL). *)
  Data.Original_exe.get_addr_size_exn obj >>= fun addr_size ->
  Patches.get_BIL name addr_size >>= fun bil ->

  (* Stash the BIL in the KB. *)
  Data.Patch.set_bil obj bil >>= fun _ ->

  Events.(send @@ Info "Done. The patch has the following BIL:");
  Events.(send @@ Rule);
  Events.(send @@ Info (Bil.to_string bil));
  Events.(send @@ Rule);

  KB.return ()
