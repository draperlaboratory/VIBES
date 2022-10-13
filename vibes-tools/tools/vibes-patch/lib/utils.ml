open Core
open Bap.Std

type region = {
  addr : int64;
  size : int64;
  offset : int64;
}

(* Search for the largest code region containing the address. *)
let find_code_region
    (loc : int64)
    (spec : Ogre.doc) : region option =
  let compare (_, s1, _) (_, s2, _) = Int64.compare s2 s1 in
  Ogre.foreach Ogre.Query.(begin
      let open Image.Scheme in
      let addr = code_region.(addr) in
      let size = code_region.(size) in
      select ~where:(addr <= int loc && int loc < addr + size)
        (from code_region)
    end) ~f:Fn.id |> Fn.flip Ogre.eval spec |> Or_error.ok |>
  Option.map ~f:Seq.to_list |>
  Option.map ~f:(List.sort ~compare) |>
  Option.bind ~f:List.hd |>
  Option.map ~f:(fun (addr, size, offset) -> {addr; size; offset})

let addr_to_offset (addr : int64) (region : region) : int64 =
  Int64.(addr - region.addr + region.offset)

let offset_to_addr (offset : int64) (region : region) : int64 =
  Int64.(offset - region.offset + region.addr)
