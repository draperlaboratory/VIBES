open !Core_kernel
open Bap_knowledge
open Knowledge.Syntax
open Bap_vibes
open OUnit2

module KB = Knowledge
module H = Helpers


(* Test that [Exe_ingester.ingest] stashes the address size in the KB. *)
let test_ingest_addr_size (_ : test_ctxt) : unit =

  (* Run the ingester. *)
  let computation =
    let proj = H.proj |> Result.map_error ~f:Error.raise |> Result.ok_exn in

    (* Set up the KB. *)
    H.obj () >>= fun obj ->
    Data.Original_exe.set_filepath obj (Some H.original_exe) >>= fun _ ->

    (* Now run the ingester. *)
    Exe_ingester.ingest obj proj >>= fun _ ->
    KB.return obj

  in
  let result = KB.run Data.cls computation KB.empty in

  (* The ingester should stash the address size in the KB. *)
  let expected = Some 32 in
  H.assert_property
    ~p_res:H.print_int_opt ~p_expected:H.print_int_opt ~cmp:(Option.equal (=))
    Data.Original_exe.addr_size expected result

let suite = [
  "Test Exe_ingester.ingest: stashes address size" >:: test_ingest_addr_size;
]
