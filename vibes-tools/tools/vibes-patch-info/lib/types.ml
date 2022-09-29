module Log = Vibes_log.Stream
module Utils = Vibes_utils
module Hvar = Vibes_higher_vars.Higher_var

type space = {
  address : Utils.Json.Bitvector.t [@key "address"];
  size : int64 [@key "size"];
} [@@deriving yojson]

type t = {
  patch_point : Utils.Json.Bitvector.t [@key "patch-point"];
  patch_size : int64 [@key "patch-size"];
  sp_align : int [@key "sp-align"];
  patch_spaces : space list [@default []] [@key "patch-spaces"];
  patch_vars : Hvar.t list [@default []] [@key "patch-vars"];
} [@@deriving yojson]

let pp = Utils.Json.pp ~yojson_of_t
let from_file = Utils.Json.from_file ~yojson_of_t ~t_of_yojson
