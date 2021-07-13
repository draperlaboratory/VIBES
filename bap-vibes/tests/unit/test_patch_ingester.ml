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
    Data.Original_exe.set_target obj H.dummy_target >>= fun _ ->
    KB.Object.create Data.Patch.patch >>= fun patch ->
    Data.Patch.set_patch_name patch (Some H.patch) >>= fun _ ->
    Data.Patched_exe.set_patches obj
      (Data.Patch_set.singleton patch) >>= fun _ ->

    (* Now run the ingester. *)
    Patch_ingester.ingest obj >>= fun _ ->
    Data.Patched_exe.get_patches obj >>= fun patches ->
    match Data.Patch_set.to_list patches with
    | [] -> assert_failure "Result patch missing."
    | (p :: []) ->
      Data.Patch.get_bir p >>= fun bir ->
      Patches.Ret_3.prog 32 >>= fun expected ->
      let open Bap.Std in
      let expected = KB.Value.get Bil.slot expected in
      let bil = KB.Value.get Bil.slot bir in
      let err =
        Format.asprintf "Expected %a but got %a"
          Bil.pp expected
          Bil.pp bil
      in
      assert_bool err (Bil.compare expected bil = 0);
      KB.return p
    | _ -> assert_failure "Multiple patches returned when one expected."

  in
  ignore @@ KB.run Data.Patch.patch computation KB.empty

(* Test that [Patch_ingester.ingest] errors with no patch name in the KB. *)
let test_ingest_with_no_patch (_ : test_ctxt) : unit =

  (* Run the ingester. *)
  let computation =
    H.obj () >>= fun obj ->
    Data.Original_exe.set_target obj H.dummy_target >>= fun () ->
    (* Create a patch but don't fill its properties. *)
    KB.Object.create Data.Patch.patch >>= fun patch ->
    Data.Patched_exe.set_patches obj
      (Data.Patch_set.singleton patch) >>= fun _ ->
    Patch_ingester.ingest obj >>= fun _ ->
    KB.return obj
  in
  let result = KB.run Data.cls computation KB.empty in

  (* The ingester should diverge with the appropriate error. *)
  let expected = Kb_error.Problem Kb_error.Missing_patch_name in
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
      (Data.Patch_set.singleton patch) >>= fun _ ->

    (* Now run the ingester. *)
    Patch_ingester.ingest obj >>= fun _ ->
    KB.return obj

  in
  let result = KB.run Data.cls computation KB.empty in

  (* The ingester should diverge with the appropriate error. *)
  let expected = Kb_error.Problem Kb_error.Missing_target in
  H.assert_error Data.Patched_exe.patches expected result

let suite = [
  "Test Patch_ingester.ingest" >:: test_ingest;
  "Test Patch_ingester.ingest: no patch" >::
  test_ingest_with_no_patch;
  "Test Patch_ingester.ingest: no address size" >::
  test_ingest_with_no_addr_size;
]
