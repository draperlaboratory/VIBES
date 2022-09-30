open Bap_core_theory

(** An external patch space.

    - [address]: the absolute address of the patch space

    - [size]: the size of the patch space
*)
type space = {
  address : Vibes_utils.Json.Bitvector.t;
  size : int64;
} [@@deriving yojson]

(** A list of external patch spaces. *)
type spaces = space list [@@deriving yojson]

(** Pretty-prints the patch spaces. *)
val pp_spaces : Format.formatter -> spaces -> unit

(** Attempts to deserialize the patch spaces from a JSON file. *)
val spaces_from_file : string -> (spaces, KB.conflict) result

(** The metadata for a patch.

    - [patch_point]: the absolute address of the program
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
