module Log = Vibes_log_lib.Stream
module Utils = Vibes_utils_lib
module Hvar = Vibes_higher_vars_lib.Higher_var

type Vibes_error_lib.Std.t +=
  | Json_parse_error of string

let printer (e : Vibes_error_lib.Std.t) : string option =
  match e with
  | Json_parse_error s -> Some s
  | _ -> None

let () = Vibes_error_lib.Std.register_printer printer

type space = {
  address : int64 [@key "address"]; 
  size : int64 [@key "size"];
} [@@deriving yojson]

type t = {
  patch_point : Utils.Json.Bitvector.t [@key "patch-point"];
  patch_size : int64 [@key "patch-size"];
  sp_align : int [@key "sp-align"];
  patch_spaces : space list option [@yojson.option] [@key "patch-spaces"];
  patch_vars : Hvar.t list option [@yojson.option] [@key "patch-vars"];
} [@@deriving yojson]

let address space : int64 = space.address
let size space : int64 = space.size

let patch_point t : Utils.Json.Bitvector.t = t.patch_point
let patch_size t : int64 = t.patch_size
let sp_align t : int = t.sp_align
let patch_spaces t : space list option = t.patch_spaces
let patch_vars t : Hvar.t list =
  match t.patch_vars with
  | Some l -> l
  | None -> []

let pp = Utils.Json.pp ~yojson_of_t
let from_file = Utils.Json.from_file ~yojson_of_t ~t_of_yojson
