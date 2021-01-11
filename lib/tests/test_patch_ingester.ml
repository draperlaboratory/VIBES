open Bap_knowledge
open Knowledge.Syntax
open Bap_vibes
open Bap_core_theory
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
    Data.Patch.set_patch_name obj (Some H.patch) >>= fun _ ->

    (* Now run the ingester. *)
    Patch_ingester.ingest obj >>= fun _ ->
    KB.return obj

  in
  let result = KB.run Data.cls computation KB.empty in

  (* The ingester should stash the patch (BIR) in the KB. *)
  let expected = Patches.Ret_3.prog 32 in
  let expected = expected >>| fun p ->
    KB.Value.put Theory.Semantics.slot Theory.Program.empty p in
  let expected =
    expected >>= fun _ -> KB.Object.create Theory.Program.cls in
  let expected = KB.run Theory.Program.cls expected KB.empty in
  let expected = Result.get_ok expected |> fst in

  H.assert_property
    ~p_res:H.print_bir ~p_expected:H.print_bir
    ~cmp:Theory.Program.equal
    Data.Patch.bir expected result


(* Test that [Patch_ingester.ingest] errors with no patch name in the KB. *)
let test_ingest_with_no_patch (_ : test_ctxt) : unit =

  (* Run the ingester. *)
  let computation =
    (* Don't set up the KB. Leave it empty. *)
    H.obj () >>= fun obj ->
    Patch_ingester.ingest obj >>= fun _ ->
    KB.return obj
  in
  let result = KB.run Data.cls computation KB.empty in

  (* The ingester should diverge with the appropriate error. *)
  let expected = Errors.Problem Errors.Missing_patch_name in
  H.assert_error ~printer:H.print_bir Data.Patch.bir expected result

(* Test that [Patch_ingester.ingest] errors with no addr_size in the KB. *)
let test_ingest_with_no_addr_size (_ : test_ctxt) : unit =

  (* Run the ingester. *)
  let computation =

    (* Add a patch name, but no address size, to the KB. *)
    H.obj () >>= fun obj ->
    Data.Patch.set_patch_name obj (Some H.patch) >>= fun _ ->

    (* Now run the ingester. *)
    Patch_ingester.ingest obj >>= fun _ ->
    KB.return obj

  in
  let result = KB.run Data.cls computation KB.empty in

  (* The ingester should diverge with the appropriate error. *)
  let expected = Errors.Problem Errors.Missing_addr_size in
  H.assert_error ~printer:H.print_bir Data.Patch.bir expected result

let suite = [
  "Test Patch_ingester.ingest" >:: test_ingest;
  "Test Patch_ingester.ingest: no patch" >::
  test_ingest_with_no_patch;
  "Test Patch_ingester.ingest: no address size" >::
  test_ingest_with_no_addr_size;
]
