(* Define CLI arguments for the OUnit tests. *)

open OUnit2

(* Make [-minizinc-model-filepath=FILEPATH] a command line parameter
   for running the tests. The value can be retrieved anytime in the tests
   by invoking {!minizinc_model_filepath test_ctxt}. *)
let minizinc_model_filepath =
  Conf.make_string "minizinc_model_filepath" "~/.vibes/model.mzn"
    "Path to a minizinc model."
