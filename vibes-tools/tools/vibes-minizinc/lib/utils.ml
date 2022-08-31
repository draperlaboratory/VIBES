open Core
open Bap_core_theory

module Filename = Stdlib.Filename
module Json = Vibes_utils.Json
module Proc = Vibes_utils.Proc
module Log = Vibes_log.Stream

let run_minizinc
    (params : Yojson.Safe.t)
    ~(model_filepath : string) : (string, KB.conflict) result =
  let params_filepath = Filename.temp_file "vibes-mzn-params" ".json" in
  let solution_filepath = Filename.temp_file "vibes-mzn-sol" ".json" in
  Log.send "MiniZinc parameters: %s" params_filepath;
  Yojson.Safe.to_file params_filepath params;
  Proc.run "minizinc" [
      "--output-mode"; "json";
      "-o"; solution_filepath;
      "--output-objective";
      "-d"; params_filepath;
      "--soln-sep"; "\"\"";
      "--search-complete-msg"; "\"\"";
      "--solver"; "chuffed";
      model_filepath;
    ] |> Result.map ~f:(fun _ -> solution_filepath)

