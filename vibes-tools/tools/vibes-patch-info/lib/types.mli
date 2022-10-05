open Bap_core_theory

(** An external patch space.

    - [address]: the virtual address of the patch point
    - [size]: the size of the patch space
*)
type space = {
  address : Vibes_utils.Json.Bitvector.t;
  size : int64;
} [@@deriving yojson]

(** A set of patch spaces. *)
module Spaces : sig

  type t [@@deriving yojson]

  (** The empty set of spaces. *)
  val empty : t

  (** Returns [true] if set of spaces is empty. *)
  val is_empty : t -> bool

  (** Converts a list of spaces into an interval tree where
      overlapping regions are coalesced. *)
  val of_list : space list -> t

  (** Returns the interval tree as a list of spaces. *)
  val to_list : t -> space list

  (** Pretty-prints the patch metadata. *)
  val pp : Format.formatter -> t -> unit

  (** Attempts to deserialize the metadata from a JSON file. *)
  val from_file : string -> (t, KB.conflict) result

end

(** The metadata for a patch.

    - [patch_point]: the virtual address of the program
      where the either the patch or a jump to the patch
      should be placed

    - [patch_size]: the number of bytes being overwritten
      by the patch

    - [sp_align]: the amount (in bytes) that needs to be
      subtracted from the stack pointer in order to keep
      it aligned when making function calls

    - [patch_vars]: higher variable information
*)
type t = {
  patch_point : Vibes_utils.Json.Bitvector.t;
  patch_size : int64;
  sp_align : int;
  patch_vars : Vibes_higher_vars.Higher_var.t list;
}

(** Pretty-prints the patch metadata. *)
val pp : Format.formatter -> t -> unit

(** Attempts to deserialize the metadata from a JSON file. *)
val from_file : string -> (t, KB.conflict) result
