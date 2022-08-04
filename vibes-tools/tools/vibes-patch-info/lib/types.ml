module Log = Vibes_log_lib.Stream
module Utils = Vibes_utils_lib
module Hvar = Vibes_higher_vars_lib.Higher_var

type space = {
  address : int64 [@key "address"]; 
  size : int64 [@key "size"];
} [@@deriving yojson, fields]

type t = {
  patch_point : Utils.Json.Bitvector.t [@key "patch-point"];
  patch_size : int64 [@key "patch-size"];
  sp_align : int [@key "sp-align"];
  patch_spaces : space list option [@yojson.option] [@key "patch-spaces"];
  patch_vars : Hvar.t list option [@yojson.option] [@key "patch-vars"];
} [@@deriving yojson, fields]

let patch_vars (t : t) : Hvar.t list =
  match t.patch_vars with
  | Some l -> l
  | None -> []

let pp = Utils.Json.pp ~yojson_of_t
let from_file = Utils.Json.from_file ~yojson_of_t ~t_of_yojson
