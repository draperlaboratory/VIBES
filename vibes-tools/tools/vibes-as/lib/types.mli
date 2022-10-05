open Core
open Bap_core_theory

module Assembly : sig

  (** A basic block of assembly code. *)
  type block = {
    label : string;
    insns : string list;
  } [@@deriving fields, sexp]

  (** An assembly program. *)
  type t = {
    patch_point : int64;
    patch_size : int64;
    overwrite : bool;
    directives : string list;
    blocks : block list;
  } [@@deriving fields, sexp]

  (** A printer will take a VIBES IR program and return the assembly
      representation if it is well-formed. *)
  type printer =
    Vibes_ir.Types.t ->
    Vibes_patch_info.Types.t ->
    (t, KB.conflict) result

  (** [pp ppf asm] will pretty-print the [asm] program to formatter
      [ppf]. *)
  val pp : Format.formatter -> t -> unit
  
end
