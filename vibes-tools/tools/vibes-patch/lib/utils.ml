open Core
open Bap.Std
open Bap_core_theory

type region = {
  addr : int64;
  size : int64;
  offset : int64;
}

let find_code_region
    (loc : int64)
    (spec : Ogre.doc) : (region, KB.conflict) result =
  Ogre.require ~that:(fun (addr, size, _) ->
      Int64.(addr <= loc && loc <= addr + size))
    Image.Scheme.code_region |>
  Fn.flip Ogre.eval spec |> function
  | Ok (addr, size, offset) -> Ok {addr; size; offset}
  | Error err ->
    let msg = Format.asprintf
        "Couldn't find patch point 0x%Lx: %a"
        loc Error.pp err in
    Error (Errors.Invalid_patch_point msg)

let addr_to_offset (addr : int64) (region : region) : int64 =
  Int64.(addr - region.addr + region.offset)

let offset_to_addr (offset : int64) (region : region) : int64 =
  Int64.(offset - region.offset + region.addr)
