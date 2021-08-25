(** This IR is intended for the eventual serialization of parameters to
    a constraint model that solves a joint instruction scheduling and
    register allocation problem in the style of Unison. It is modeled
    roughly after the Unison IR as described in chapter 5 of the Unison
    manual.

    Because the IR needs to be serialized to an external solver, many
    entities require ids.

    Unison makes the distinction between operands and temporaries.  An
    operand is a field of an operation. In other words operands belong
    to a single operation.  One operand may have multiple temporaries
    available from which to choose from. Temporaries do not belong to a
    single operation.  This conceptual separation opens up enough space
    to model spilling registers and live range splitting.  An operand
    may be preassigned to a particular register.

    Unison also makes a distinction between an operation and an
    instruction.  An operation may be implemented via different
    instructions.  Instructions correspond typically to assembly
    instructions like mov.

    It is intended that this IR be put into linear SSA form. In linear
    SSA temporaries are uniquely assigned and belong uniquely to a
    single block.  Temporaries that persist across blocks are recorded
    as congruent in the [vibes_ir] type.  This can be achived by
    namespacing variables by the block they belong to.  The purpose of
    the linear SSA is to help the constraint satisfaction problem
    conceptually decompose into coupled block level constraint
    satisfaction problems. *)

open Bap.Std
open Bap_core_theory

type opcode [@@deriving compare, equal, sexp]

module Opcode :
sig

  type t = opcode

  val create : ?arch:string -> string -> opcode

  val name : opcode -> string

  val (=) : opcode -> opcode -> bool

end


(** [operand]s have unique ids, a list of potential temporaries that
    can be used to implement the operand and may be optionally
    pre-assigned to registers for calling conventions or other reasons. *)
type op_var = {
  id : var;
  temps : var list;
  pre_assign : var option
} [@@deriving compare, equal, sexp]

val simple_var : var -> op_var

val given_var : var -> reg:var -> op_var

type operand = Var of op_var
             | Const of word
             | Label of tid
             | Void of op_var
             | Offset of word [@@deriving compare, equal, sexp]

(** An [operation] has
    a *unique* id
    an assigned lhs,
    a set of instructions to choose from,
    a flag of whether the operation is optional,
    a list of operands. *)
type operation = {
  id : tid;
  lhs : operand list;
  opcodes : opcode list;
  optional : bool;
  operands : operand list;
} [@@deriving compare, equal, sexp]

val simple_op : opcode -> operand -> operand list -> operation

(** A [vibes_blk] has an id,
    a set of operations,
    a set of input temporaries assumed to exists at the beginning of the block,
    a set of output temporaries assumed to persist out the end of the block,
    an estimated execution frequency. *)
type blk = {
  id : tid;
  data : operation list;
  ctrl : operation list;
  ins : operation;
  outs : operation;
  frequency : int
} [@@deriving compare, equal, sexp]

(** Create a block given a [tid] and a list of [operation]s, filling
    in default values for the other fields. *)
val simple_blk : tid -> data:(operation list) -> ctrl:(operation list) -> blk

(** The [vibes_ir] type has a list of blocks and a set of operands
    which are congruent. *)
type t = {
  blks : blk list;
  congruent : (op_var * op_var) list
} [@@deriving compare, equal, sexp]

val empty : t

val union : t -> t -> t

val add : blk -> t -> t


val map_blks : f:(blk -> blk) -> t -> t
val map_op_vars : f:(op_var -> op_var) -> t -> t
val map_operations : f:(operation -> operation) -> t -> t

val operation_to_string : operation -> string
val op_var_to_string : op_var -> string

val all_temps : t -> Var.Set.t
val all_operands : t -> Var.Set.t

(** [preassign tgt ir] sets all the variables which are set to
    registers in [tgt] as being the preassigned location for those
    variables. *)
val preassign : Theory.target -> t -> t

(** [preassign_map] builds a total dictionary from op_var ids to
    pre assigned registers. *)
val preassign_map : t -> (var option) Var.Map.t

(** [definer_map] takes a subroutine and builds a Map from all
    temporaries to the unique lhs operand where that temporary is
    defined.*)
val definer_map : t -> op_var Var.Map.t

(** [users_map] takes a subroutine and builds a Map from all
    temporaries to the operands that may use that temporary. *)
val users_map : t -> (op_var list) Var.Map.t

(** [temp_blk] builds a Map from temporaries to the unique block in
    which they are defined and used. *)
val temp_blk : t -> Tid.t Var.Map.t

val operation_opcodes : t -> opcode list Tid.Map.t
val operand_operation : t -> operation Var.Map.t
val pretty_ir : t -> string
(* Alias of pretty_ir *)
val to_string : t -> string

val op_var_exn : operand -> op_var

(** Populate the [pre_assign] field with [`R0] if it is not already
    assigned. Useful for testing purposes. *)
val dummy_reg_alloc : t -> t
