(* Implements {!Patch_ingester}. *)

open Bap.Std
open Bap_knowledge
open Knowledge.Syntax

module KB = Knowledge

(* Loads the BIL version of a patch. For now, we select from a hand-written
   set of patches defined in the {!Patches} module. *)
let ingest_one (addr_size : int) (n : int KB.t) (patch : Data.Patch.t)
    : int KB.t =
  n >>= fun patch_num ->
  Data.Patch.get_patch_name_exn patch >>= fun name ->
  Events.(send @@ Info (Printf.sprintf "\nIngesting patch %d." patch_num));
  Events.(send @@ Info (Printf.sprintf "Selecting patch named: %s" name));

  (* Get the patch (as BIL). *)
  Patches.get_BIL name addr_size >>= fun bil ->

  (* Stash the BIL in the KB. *)
  Data.Patch.set_bil patch bil >>= fun _ ->

  Events.(send @@ Info "The patch has the following BIL:");
  Events.(send @@ Rule);
  Events.(send @@ Info (Bil.to_string bil));
  Events.(send @@ Rule);
  KB.return (patch_num+1)

let ingest (obj : Data.t) : unit KB.t =
  Events.(send @@ Header "Starting patch ingester");
  Events.(send @@ Info "Using hand-written BIL patches");

  Events.(send @@ Info "Retreiving data from KB...");
  Data.Original_exe.get_addr_size_exn obj >>= fun addr_size ->
  Data.Patched_exe.get_patches obj >>= fun patches ->
  Events.(send @@ Info (Printf.sprintf "There are %d patches"
                          (Data.PatchSet.length patches)));
  Data.PatchSet.fold patches ~init:(KB.return 1)
    ~f:(ingest_one addr_size) >>= fun _ ->

  Events.(send @@ Info "Patch ingest complete");
  Events.(send @@ Rule);
  KB.return ()

