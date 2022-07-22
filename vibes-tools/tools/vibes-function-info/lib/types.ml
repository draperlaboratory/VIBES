open Core
open Bap.Std

module T = Bap_core_theory.Theory
module Utils = Vibes_utils_lib

type func = {
  label : Utils.Json.Label.t;
  args : string list;
} [@@deriving yojson, equal, compare]

type t = {
  functions : func list;
} [@@deriving yojson, equal, compare]

let pp = Utils.Json.pp ~yojson_of_t
let from_file = Utils.Json.from_file ~yojson_of_t ~t_of_yojson
let to_string t = Yojson.Safe.to_string (yojson_of_t t)

let empty = { functions = [] }
let create_func label args : func = { label; args }
let create functions : t = { functions }

let append ~(func_info : func) t : t =
  { functions = List.append t.functions [func_info] }

let vars_of_args ~(target : T.Target.t) (args : string list) : var list =
  let word_size = Type.Imm (T.Target.bits target) in
  List.map args ~f:(fun arg -> Var.create arg word_size)

let find ~(tid : Tid.t) t : string list option =
  let rec aux tid funcs = 
    match funcs with
    | [] -> None
    | hd :: tl ->
      if Tid.equal tid hd.label then Some hd.args
      else aux tid tl
  in
  aux tid t.functions
