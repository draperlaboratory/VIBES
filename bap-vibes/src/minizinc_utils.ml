(***************************************************************************)
(*                                                                         *)
(*  Copyright (C) 2022/2023 The Charles Stark Draper Laboratory, Inc.      *)
(*                                                                         *)
(*  This file is provided under the license found in the LICENSE file in   *)
(*  the top-level directory of this project.                               *)
(*                                                                         *)
(*  This research was developed with funding from the Defense Advanced     *)
(*  Research Projects Agency (DARPA).                                      *)
(*                                                                         *)
(***************************************************************************)

open Bap.Std
open Core_kernel

open Bap_knowledge
module KB = Knowledge
open Knowledge.Syntax
open Bap_core_theory


(* Convenience types for minizinc serialization. At this point nearly everything
   becomes stringly typed. The {set :} and {e : } wrappers produce the correct json
   for serialization to minzinc *)
type 'a mzn_set = {set : 'a list}  [@@deriving yojson]
type ('a ,'b) mzn_map = 'b list

(* Phantom types make yojson_deriving produce function with unused variables.
   This sets off a warning *)
let mzn_map_of_yojson = fun _ -> [%of_yojson: 'b list]
let mzn_map_to_yojson = fun _ -> [%to_yojson: 'b list]

type mzn_enum = {e : string} [@@deriving yojson]
type mzn_enum_def = mzn_enum mzn_set [@@deriving yojson] (* https://github.com/MiniZinc/libminizinc/issues/441 *)

let mzn_enum (x : string) : mzn_enum = {e = x}
let mzn_enum_def_of_list (tags : string list) : mzn_enum_def = {set = List.map ~f:mzn_enum tags}
let mzn_set_of_list l = {set = l}

let mzn_enum_of_var (v : var) : mzn_enum = Var.sexp_of_t v |> Sexp.to_string |> mzn_enum


let run_minizinc
    (params : Yojson.Safe.t)
    ~model_filepath:(model_filepath : string) =
  let params_filepath =
    Stdlib.Filename.temp_file "vibes-mzn-params" ".json" in
  let solution_filepath =
    Stdlib.Filename.temp_file "vibes-mzn-sol" ".json" in
  Events.(send @@ Info (sprintf "Paramfile: %s\n" params_filepath));
  Yojson.Safe.to_file params_filepath params;
  let minizinc_args = ["--output-mode"; "json";
                       "-o"; solution_filepath;
                       "--output-objective";
                       "-d"; params_filepath;
                       "--soln-sep"; "\"\"";  (* Suppress some unwanted annotations *)
                       "--search-complete-msg";"\"\"";
                       "--solver"; "chuffed";
                       model_filepath ] in
  Utils.lift_kb_result (Utils.run_process "minizinc" minizinc_args) >>= fun () ->
  KB.return (Yojson.Safe.from_file solution_filepath)
