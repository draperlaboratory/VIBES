open Core
open Bap.Std

module S = Image.Scheme

type region = {
  addr   : int64;
  size   : int64;
  offset : int64;
}

type named_region = {
  addr : int64;
  size : int64;
  name : string;
}

(* Search for the largest code region containing the address. *)
let find_code_region
    (loc : int64)
    (spec : Ogre.doc) : region option =
  let compare (_, s1, _) (_, s2, _) = Int64.compare s2 s1 in
  Ogre.collect Ogre.Query.(begin
      let open S in
      let addr = code_region.(addr) in
      let size = code_region.(size) in
      select ~where:(addr <= int loc && int loc < addr + size)
        (from code_region)
    end) |> Fn.flip Ogre.eval spec |> Or_error.ok |>
  Option.map ~f:Seq.to_list |>
  Option.map ~f:(List.sort ~compare) |>
  Option.bind ~f:List.hd |>
  Option.map ~f:(fun (addr, size, offset) -> {addr; size; offset})

(* Search for the largest mapped region containing the address. *)
let find_mapped_region
    (loc : int64)
    (spec : Ogre.doc) : region option =
  let compare {S.size=s1;_} {S.size=s2;_} = Int64.compare s2 s1 in
  Ogre.collect Ogre.Query.(begin
      let open Image.Scheme in
      let addr = mapped.(addr) in
      let size = mapped.(size) in
      select ~where:(addr <= int loc && int loc < addr + size)
        (from mapped)
    end) |> Fn.flip Ogre.eval spec |> Or_error.ok |>
  Option.map ~f:Seq.to_list |>
  Option.map ~f:(List.sort ~compare) |>
  Option.bind ~f:List.hd |>
  Option.map ~f:(fun {S.addr; size; info=offset} -> {addr; size; offset})

(* Search for the largest named region containing the address. *)
let find_named_region (loc : int64) (spec : Ogre.doc) : named_region option =
  let compare {S.size=s1;_} {S.size=s2;_} = Int64.compare s2 s1 in
  Ogre.collect Ogre.Query.(begin
      let open S in
      let addr = named_region.(addr) in
      let size = named_region.(size) in
      select ~where:(addr <= int loc && int loc < addr + size)
        (from named_region)
    end) |> Fn.flip Ogre.eval spec |> Or_error.ok |>
  Option.map ~f:Seq.to_list |>
  Option.map ~f:(List.sort ~compare) |>
  Option.bind ~f:List.hd |>
  Option.map ~f:(fun {S.addr; size; info=name} -> {addr; size; name})

(* Find the name of the symbol for the address. *)
let find_named_symbol (loc : int64) (spec : Ogre.doc) : string option =
  Ogre.require S.named_symbol ~that:(fun (addr, _) -> Int64.(addr = loc)) |>
  Fn.flip Ogre.eval spec |> Or_error.ok |> Option.map ~f:snd

let addr_to_offset (addr : int64) (region : region) : int64 =
  Int64.(addr - region.addr + region.offset)

let offset_to_addr (offset : int64) (region : region) : int64 =
  Int64.(offset - region.offset + region.addr)
