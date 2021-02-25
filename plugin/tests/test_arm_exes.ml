open OUnit2

module H = Helpers

(* Check patching the "simple" exe example. *)
let test_simple (ctxt : test_ctxt) : unit =
  H.run_make ["clean"] ~dir:"arm-simple" ~ctxt;
  H.run_make ["patch"] ~dir:"arm-simple" ~ctxt;
  H.run_arm_exe ["main"] ~dir:"arm-simple" ~exit_code:5 ~ctxt;
  H.run_arm_exe ["main.patched"] ~dir:"arm-simple" ~exit_code:3 ~ctxt;
  H.run_make ["clean"] ~dir:"arm-simple" ~ctxt

(* Check patching the "simple-cegis" exe example. *)
let test_simple_cegis (ctxt : test_ctxt) : unit =
  H.run_make ["clean"] ~dir:"simple-cegis" ~ctxt;
  H.run_make ["patch"] ~dir:"simple-cegis" ~ctxt;
  H.run_arm_exe ["main"] ~dir:"simple-cegis" ~exit_code:5 ~ctxt;
  H.run_arm_exe ["main.patched"] ~dir:"simple-cegis" ~exit_code:3 ~ctxt;
  H.run_make ["clean"] ~dir:"simple-cegis" ~ctxt

(* Check patching the "simple-compiled" exe example. *)
let test_simple_compiled (ctxt : test_ctxt) : unit =
  H.run_make ["clean"] ~dir:"arm-simple-compiled" ~ctxt;
  H.run_make ["patch"] ~dir:"arm-simple-compiled" ~ctxt;
  H.run_arm_exe ["main"] ~dir:"arm-simple-compiled" ~exit_code:5 ~ctxt;
  H.run_arm_exe ["main.patched"] ~dir:"arm-simple-compiled" ~exit_code:3 ~ctxt;
  H.run_make ["clean"] ~dir:"arm-simple-compiled" ~ctxt

(* Check patching the "simple-multi" exe example. *)
let test_simple_multi (ctxt : test_ctxt) : unit =
  H.run_make ["clean"] ~dir:"arm-simple-multi" ~ctxt;
  H.run_make ["patch"] ~dir:"arm-simple-multi" ~ctxt;
  H.run_arm_exe ["main"] ~dir:"arm-simple-multi" ~exit_code:0 ~ctxt;
  H.run_arm_exe ["main.patched"] ~dir:"arm-simple-multi" ~exit_code:1 ~ctxt;
  H.run_make ["clean"] ~dir:"arm-simple-multi" ~ctxt

let suite = [
  "Test: simple" >:: test_simple;
  "Test: simple-cegis" >:: test_simple_cegis;
  "Test: simple-compiled" >:: test_simple_compiled;
  "Test: simple-multi" >:: test_simple_multi;
]
