module T = Bap_core_theory.Theory
module Json = Yojson.Safe
module Log = Vibes_log_lib.Stream
module Err = Vibes_error_lib.Std

open Vibes_error_lib.Let

module Bitvector = struct

  include Yojson.Safe
  module Word = Bap.Std.Word

  type t = Word.t
  let equal = Word.equal
  let compare = Word.compare

  let t_of_yojson json =
    match json with
    | `String s -> Word.of_string s
    | _ -> failwith "Invalid hex/bitvector string"

  let yojson_of_t t = `String (Word.to_string t)

end

module Label = struct

  include Yojson.Safe

  type t = T.Label.t
  let equal = Bap.Std.Tid.equal
  let compare = Bap.Std.Tid.compare

  let t_of_yojson json =
    match json with
    | `String s ->
      begin
        match Bap.Std.Tid.from_string s with
        | Ok s -> s
        | Error _ -> failwith (Format.sprintf "No such label '%s'" s)
      end
    | _ -> failwith "Invalid label string"

  let yojson_of_t t = `String (Bap.Std.Tid.to_string t)

end

let pp ~yojson_of_t fmt t =
  let json = yojson_of_t t in
  Json.pretty_print fmt json

let from_file ~yojson_of_t ~t_of_yojson filepath =
  let- json =
    try Ok (Json.from_file filepath)
    with _ ->
      let msg = Format.sprintf "Couldn't load JSON file: '%s'" filepath in
      Error (Types.Json_parse_error msg)
  in
  try
    let data = t_of_yojson json in
    let this_pp = pp ~yojson_of_t in
    Log.send @@ Format.asprintf "Loaded: %a" this_pp data;
    Ok data
  with _ ->
    let msg = Format.sprintf "Couldn't deserialize JSON in '%s'" filepath in
    Error (Types.Json_deserialization_error msg)
