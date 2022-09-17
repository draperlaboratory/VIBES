open Core
open Bap.Std

type region = {
  addr : int64;
  size : int64;
  offset : int64;
}

let find_code_region
    (loc : int64)
    (spec : Ogre.doc) : region option =
  Ogre.require ~that:(fun (addr, size, _) ->
      Int64.(addr <= loc && loc <= addr + size))
    Image.Scheme.code_region |>
  Fn.flip Ogre.eval spec |> function
  | Ok (addr, size, offset) -> Some {addr; size; offset}
  | Error _ -> None

let addr_to_offset (addr : int64) (region : region) : int64 =
  Int64.(addr - region.addr + region.offset)

let offset_to_addr (offset : int64) (region : region) : int64 =
  Int64.(offset - region.offset + region.addr)
