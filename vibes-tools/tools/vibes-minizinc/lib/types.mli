open Core
open Bap.Std
open Bap_core_theory

module Ir = Vibes_ir.Types

(** Retains auxiliary information which is primarily useful for
    interpreting the solution returned by MiniZinc. In particular it
    retains the ordering of temporaries, operations, and operands.
    This is important because a MiniZinc map is represented as an array. *)
type info = {
  temps : Var.t list;
  temp_map : Var.t String.Map.t;
  reg_map : Var.t String.Map.t;
  operations : Ir.id list;
  operands : Ir.id list;
} [@@deriving equal]

(** Representation of the solution returned by MiniZinc. *)
module Solution : sig

  type t = {
    reg : var Var.Map.t;
    opcode : Ir.opcode Ir.id_map;
    temp : var Ir.id_map;
    active : bool Ir.id_map;
    issue : int Ir.id_map;
  } [@@deriving sexp, compare]

  include Comparator.S with type t := t

  type set = (t, comparator_witness) Set.t

  val empty_set : set

  (** [deserialize filename info] parses the solution produced by MiniZinc
      to the file [filename], and interprets the data according to [info]. *)
  val deserialize : string -> info -> (t, KB.conflict) result

  (** [apply ir solution] applies the [solution] to the [ir] program. *)
  val apply : Ir.t -> t -> Ir.t

end

module Params : sig

  (** [serialize ir target ?prev_solutions] will translate the IR program
      into the JSON format for consumption by MiniZinc, along with
      auxilliary information for interpreting the solution. *)
  val serialize :
    ?prev_solutions:Solution.set ->
    Ir.t ->
    Theory.target ->
    (Yojson.Safe.t * info, KB.conflict) result

end
