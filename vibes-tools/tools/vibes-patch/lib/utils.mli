open Core
open Bap_core_theory

(** A region in the binary. *)
type region = {
  addr : int64;
  size : int64;
  offset : int64;
}

(** [find_code_region addr spec] looks up the code region in [spec] that
    contains [addr]. *)
val find_code_region : int64 -> Ogre.doc -> (region, KB.conflict) result

(** Converts a virtual address into a file offset. Assumes that the address
    is contained within the region. *)
val addr_to_offset : int64 -> region -> int64

(** Converts a file offset into a virtual address. Assumes that the offset
    is contained within the region. *)
val offset_to_addr : int64 -> region -> int64
