open OUnit2

module H = Helpers

(* Check patching the "simple" exe example. *)
let test_simple (ctxt : test_ctxt) : unit =
  H.run_make ["clean"] ~dir:"simple" ~ctxt;
  H.run_make ["patch"] ~dir:"simple" ~ctxt;
  H.run_arm_exe ["main"] ~dir:"simple" ~exit_code:5 ~ctxt;
  H.run_arm_exe ["main.patched"] ~dir:"simple" ~exit_code:3 ~ctxt;
  H.run_make ["clean"] ~dir:"simple" ~ctxt

(* Check patching the "simple-compiled" exe example. *)
let test_simple_compiled (ctxt : test_ctxt) : unit =
  H.run_make ["clean"] ~dir:"simple-compiled" ~ctxt;
  H.run_make ["patch"] ~dir:"simple-compiled" ~ctxt;
  H.run_arm_exe ["main"] ~dir:"simple-compiled" ~exit_code:5 ~ctxt;
  H.run_arm_exe ["main.patched"] ~dir:"simple-compiled" ~exit_code:3 ~ctxt;
  H.run_make ["clean"] ~dir:"simple-compiled" ~ctxt

(* Check patching the "simple-multi" exe example. *)
let test_simple_multi (ctxt : test_ctxt) : unit =
  H.run_make ["clean"] ~dir:"simple-multi" ~ctxt;
  H.run_make ["patch"] ~dir:"simple-multi" ~ctxt;
  H.run_arm_exe ["main"] ~dir:"simple-multi" ~exit_code:0 ~ctxt;
  H.run_arm_exe ["main.patched"] ~dir:"simple-multi" ~exit_code:1 ~ctxt;
  H.run_make ["clean"] ~dir:"simple-multi" ~ctxt

let suite = [
  "Test: simple" >:: test_simple;
  "Test: simple-compiled" >:: test_simple_compiled;
  "Test: simple-multi" >:: test_simple_multi;
]
