open Bap_core_theory

module Log = Vibes_log.Stream
module Utils = Vibes_utils
module Hvar = Vibes_higher_vars.Higher_var

type space = {
  address : Utils.Json.Bitvector.t [@key "address"];
  size : int64 [@key "size"];
} [@@deriving yojson]

type spaces = space list [@@deriving yojson]

let pp_spaces : Format.formatter -> spaces -> unit =
  Utils.Json.pp ~yojson_of_t:yojson_of_spaces

let spaces_from_file : string -> (spaces, KB.conflict) result =
  Utils.Json.from_file
    ~yojson_of_t:yojson_of_spaces
    ~t_of_yojson:spaces_of_yojson

type t = {
  patch_point : Utils.Json.Bitvector.t [@key "patch-point"];
  patch_size : int64 [@key "patch-size"];
  sp_align : int [@key "sp-align"];
  patch_vars : Hvar.t list [@default []] [@key "patch-vars"];
} [@@deriving yojson]

let pp : Format.formatter -> t -> unit = Utils.Json.pp ~yojson_of_t

let from_file : string -> (t, KB.conflict) result =
  Utils.Json.from_file ~yojson_of_t ~t_of_yojson
