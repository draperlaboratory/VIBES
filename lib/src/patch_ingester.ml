(* Implements {!Patch_ingester}. *)

open Bap.Std
open Bap_knowledge
open Knowledge.Syntax

module KB = Knowledge

(* Loads the BIR version of a patch. For now, we select from a hand-written
   set of patches defined in the {!Patches} module. *)
let ingest_one (addr_size : int) (n : int KB.t) (patch : Data.Patch.t)
    : int KB.t =
  n >>= fun patch_num ->
  Data.Patch.get_patch_name_exn patch >>= fun name ->
  Events.(send @@ Info (Printf.sprintf "\nIngesting patch %d." patch_num));
  Events.(send @@ Info (Printf.sprintf "Selecting patch named: %s" name));

  (* Get the patch (as BIL). *)
  Patches.get_bir name addr_size >>= fun bir ->

  (* Stash the BIL in the KB. *)
  Data.Patch.set_bir patch bir >>= fun () ->

  Events.(send @@ Info "The patch has the following BIL:");
  Events.(send @@ Rule);
  let bir_str = Format.asprintf "%a" Bil.pp (KB.Value.get Bil.slot bir) in
  Events.(send @@ Info bir_str);
  Events.(send @@ Rule);
  KB.return (patch_num+1)

let ingest (obj : Data.t) : unit KB.t =
  Events.(send @@ Header "Starting patch ingester");
  Events.(send @@ Info "Using hand-written BIL patches");

  Events.(send @@ Info "Retreiving data from KB...");
  Data.Original_exe.get_addr_size_exn obj >>= fun addr_size ->
  Data.Patched_exe.get_patches obj >>= fun patches ->
  Events.(send @@ Info (Printf.sprintf "There are %d patches"
                          (Data.Patch_set.length patches)));
  Data.Patch_set.fold patches ~init:(KB.return 1)
    ~f:(ingest_one addr_size) >>= fun _ ->

  Events.(send @@ Info "Patch ingest complete");
  Events.(send @@ Rule);
  KB.return ()

