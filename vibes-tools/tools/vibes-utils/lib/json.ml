open Core
open Bap_core_theory

module T = Theory
module Json = Yojson.Safe
module Log = Vibes_log.Stream
module Y = Ppx_yojson_conv_lib

module Bitvector = struct

  include Yojson.Safe
  module Word = Bap.Std.Word

  type t = Word.t
  let equal = Word.equal
  let compare = Word.compare

  let t_of_yojson = function
    | `String s -> Word.of_string s
    | _ -> failwith "Invalid hex/bitvector string"

  let yojson_of_t t = `String (Word.to_string t)

end

module Label = struct

  include Yojson.Safe

  type t = string
  let equal = String.equal
  let compare = String.compare

  let t_of_yojson = function
    | `String s -> s
    | _ -> failwith "Invalid label string"

  let yojson_of_t s = `String s

end

let pp ~yojson_of_t fmt t =
  let json = yojson_of_t t in
  Json.pretty_print fmt json

let (let*) x f = Result.bind x ~f

let from_file ~yojson_of_t ~t_of_yojson filepath =
  let* json =
    try Ok (Json.from_file filepath)
    with Y.Yojson_conv.Of_yojson_error (exn, _) ->
      let msg = Format.asprintf
          "Couldn't load JSON file '%s': %a"
          filepath Exn.pp exn in
      Error (Errors.Json_parse_error msg) in
  try
    let data = t_of_yojson json in
    Log.send "Loaded: %a" (pp ~yojson_of_t) data;
    Ok data
  with Y.Yojson_conv.Of_yojson_error (exn, _) ->
    let msg = Format.asprintf
        "Couldn't deserialize JSON in '%s': %a"
        filepath Exn.pp exn in
    Error (Errors.Json_deserialization_error msg)
