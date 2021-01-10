open Bap.Std
open Bap_knowledge
open Knowledge.Syntax
open Bap_vibes
open OUnit2

module KB = Knowledge
module H = Helpers


(* Test that [Patch_ingester.ingest] works as expected. *)
let test_ingest (_ : test_ctxt) : unit =

  (* Run the ingester. *)
  let computation =

    (* Set up the KB. *)
    H.obj () >>= fun obj ->
    Data.Original_exe.set_addr_size obj (Some 32) >>= fun _ ->
    KB.Object.create Data.Patch.patch >>= fun patch ->
    Data.Patch.set_patch_name patch (Some H.patch) >>= fun _ ->
    Data.Patched_exe.set_patches obj
      (Data.PatchSet.singleton patch) >>= fun _ ->

    (* Now run the ingester. *)
    Patch_ingester.ingest obj >>= fun _ ->
    Data.Patched_exe.get_patches obj >>= fun patches ->
    match Data.PatchSet.to_list patches with
    | [] -> assert_failure "Result patch missing."
    | (p :: []) -> KB.return p
    | _ -> assert_failure "Multiple patches returned when one expected."

  in
  let result = KB.run Data.Patch.patch computation KB.empty in
  (* The ingester should stash the patch (BIL) in the KB. *)
  let expected = Patches.Ret_3.bil 32 in
  H.assert_property
    ~p_res:H.print_bil ~p_expected:H.print_bil
    ~cmp:(fun a b -> Bil.compare a b = 0)
    Data.Patch.bil expected result

(* Test that [Patch_ingester.ingest] errors with no patch name in the KB. *)
let test_ingest_with_no_patch (_ : test_ctxt) : unit =

  (* Run the ingester. *)
  let computation =
    H.obj () >>= fun obj ->
    Data.Original_exe.set_addr_size obj (Some 32) >>= fun () ->
    (* Create a patch but don't fill its properties. *)
    KB.Object.create Data.Patch.patch >>= fun patch ->
    Data.Patched_exe.set_patches obj
      (Data.PatchSet.singleton patch) >>= fun _ ->
    Patch_ingester.ingest obj >>= fun _ ->
    KB.return obj
  in
  let result = KB.run Data.cls computation KB.empty in

  (* The ingester should diverge with the appropriate error. *)
  let expected = Errors.Problem Errors.Missing_patch_name in
  H.assert_error Data.Patched_exe.patches expected result

(* Test that [Patch_ingester.ingest] errors with no addr_size in the KB. *)
let test_ingest_with_no_addr_size (_ : test_ctxt) : unit =

  (* Run the ingester. *)
  let computation =

    (* Add a patch with a name, but no address size, to the KB. *)
    H.obj () >>= fun obj ->
    KB.Object.create Data.Patch.patch >>= fun patch ->
    Data.Patch.set_patch_name patch (Some H.patch) >>= fun _ ->
    Data.Patched_exe.set_patches obj
      (Data.PatchSet.singleton patch) >>= fun _ ->

    (* Now run the ingester. *)
    Patch_ingester.ingest obj >>= fun _ ->
    KB.return obj

  in
  let result = KB.run Data.cls computation KB.empty in

  (* The ingester should diverge with the appropriate error. *)
  let expected = Errors.Problem Errors.Missing_addr_size in
  H.assert_error Data.Patched_exe.patches expected result

let suite = [
  "Test Patch_ingester.ingest" >:: test_ingest;
  "Test Patch_ingester.ingest: no patch" >::
  test_ingest_with_no_patch;
  "Test Patch_ingester.ingest: no address size" >::
  test_ingest_with_no_addr_size;
]
