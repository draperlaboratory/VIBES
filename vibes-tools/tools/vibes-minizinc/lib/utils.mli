open Bap_core_theory

(** [run_minizinc params ~model_filepath] runs the MiniZinc process on
    the parameters [params] according to the model at [model_filepath].

    On success, the filepath to the solution (a JSON file) will be
    returned.
*)
val run_minizinc :
  Yojson.Safe.t ->
  model_filepath:string ->
  (string, KB.conflict) result
