open OUnit2

module H = Helpers

(* Check patching the "simple" exe example. *)
let test_simple (ctxt : test_ctxt) : unit =
  H.run_make ["patch"] ~dir:"simple" ~ctxt;
  H.run_arm_exe ["main"] ~dir:"simple" ~exit_code:5 ~ctxt;
  H.run_arm_exe ["main.patched"] ~dir:"simple" ~exit_code:3 ~ctxt

(* Check patching the "simple-compiled" exe example. *)
let test_simple_compiled (ctxt : test_ctxt) : unit =
  H.run_make ["patch"] ~dir:"simple-compiled" ~ctxt;
  H.run_arm_exe ["main"] ~dir:"simple-compiled" ~exit_code:5 ~ctxt;
  H.run_arm_exe ["main.patched"] ~dir:"simple-compiled" ~exit_code:3 ~ctxt

(* Check patching the "multi" exe example. *)
let test_multi (ctxt : test_ctxt) : unit =
  H.run_make ["patch"] ~dir:"simple-compiled" ~ctxt;
  H.run_arm_exe ["main"] ~dir:"simple-compiled" ~exit_code:5 ~ctxt;
  H.run_arm_exe ["main.patched"] ~dir:"simple-compiled" ~exit_code:3 ~ctxt

let suite = [
  "Test: simple" >:: test_simple;
  "Test: simple-compiled" >:: test_simple_compiled;
  "Test: multi" >:: test_multi;
]
