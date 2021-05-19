open OUnit2

let suite = "Full suite" >::: [ "Minizinc" >::: Test_minizinc.suite ]

let _ = run_test_tt_main suite
