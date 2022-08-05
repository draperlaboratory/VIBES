open Core
open Bap.Std
open Bap_core_theory

module T = Bap_core_theory.Theory
module Utils = Vibes_utils_lib

open KB.Syntax

module Bitvec = struct

  include Bitvec

  let yojson_of_t b = `String (to_string b)

  let t_of_yojson = function
    | `String s -> Bitvec.of_string s
    | _ -> failwith "Invalid bitvec"

end

type func = {
  label : Utils.Json.Label.t;
  name : string option;
  addr : Bitvec.t option;
  args : string list;
} [@@deriving yojson, equal, compare]

type t = {
  functions : func list;
} [@@deriving yojson, equal, compare]

let pp : Format.formatter -> t -> unit  =
  Utils.Json.pp ~yojson_of_t

let from_file : string -> (t, KB.conflict) result =
  Utils.Json.from_file ~yojson_of_t ~t_of_yojson

let to_string (t : t) : string =
  Yojson.Safe.to_string (yojson_of_t t)

let empty : t = {functions = []}

let create_func
    ?(name : string option)
    ?(addr : Bitvec.t option)
    (tid : tid)
    (args : string list) : func =
  let label = Tid.to_string tid in
  {label; name; addr; args}

let create functions : t = {functions}

let append (t : t) ~(func_info : func) : t =
  {functions = List.append t.functions [func_info]}

let find_args (t : t) ~(tid : tid) : string list option KB.t =
  KB.List.find_map t.functions ~f:(fun {name; args; _} ->
      let+ tid_name = KB.collect T.Label.name tid in
      match tid_name, name with
      | Some a, Some b when String.(a = b) -> Some args
      | _ -> None)
