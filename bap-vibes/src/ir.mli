(***************************************************************************)
(*                                                                         *)
(*  Copyright (C) 2022/2023 The Charles Stark Draper Laboratory, Inc.      *)
(*                                                                         *)
(*  This file is provided under the license found in the LICENSE file in   *)
(*  the top-level directory of this project.                               *)
(*                                                                         *)
(*  This research was developed with funding from the Defense Advanced     *)
(*  Research Projects Agency (DARPA).                                      *)
(*                                                                         *)
(***************************************************************************)

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

open !Core_kernel
open Bap.Std
open Bap_core_theory

type opcode [@@deriving compare, equal, sexp]

type temp = Var.t [@@deriving compare, sexp]

module Opcode :
sig

  type t = opcode

  val create : ?arch:string -> string -> opcode

  val name : opcode -> string

  val (=) : opcode -> opcode -> bool

  module Set : Set.S with type Elt.t = t

  module Map : Map.S with type Key.t = t

end

(** The "dummy" role, which is the role associated with a variable
    which does not to be assigned to a physical location, but must
    still appear in the IR to satisfy ordering constraints. *)
val dummy_role : Theory.role

(** The "preassigned" role, which is the role associated with a
    variable which already has a physical location, which is
    potentially *not* a general purpose register, which is usually
    excluded from the class_t for that variable. *)
val preassigned : Theory.role

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

type operand =
  | Var of op_var
  | Const of word
  | Label of tid
  | Void of op_var
  | Offset of word
[@@deriving compare, equal, sexp]

val create_id : unit -> int

(** An [operation] has
    a *unique* id
    an assigned lhs,
    a set of instructions to choose from,
    a flag of whether the operation is optional,
    a list of operands. *)
type operation_id = int
type operation = {
  id : operation_id;
  lhs : operand list;
  opcodes : opcode list;
  optional : bool;
  operands : operand list;
} [@@deriving compare, equal, sexp]

val simple_op : opcode -> operand -> operand list -> operation
val empty_op : unit -> operation
val write_multiple_op : opcode -> operand list -> operand list -> operation

val op_no_args : opcode -> operation

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
  congruent : (temp * temp) list
} [@@deriving compare, equal, sexp]

val empty : t

val union : t -> t -> t

val add : blk -> t -> t


val map_blks : f:(blk -> blk) -> t -> t
val map_op_vars : f:(op_var -> op_var) -> t -> t
val map_operations : f:(operation -> operation) -> t -> t
val map_operands : f:(operand -> operand) -> t -> t

val operation_to_string : operation -> string
val op_var_to_string : op_var -> string

module Blk :
sig
    val all_temps : blk -> Var.Set.t
end

val all_temps : t -> Var.Set.t
val all_operands : t -> Var.Set.t

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

(** [op_classes] maps a variable (operand) and an opcode to the role
   of the register which may implement that variable. *)
val op_classes : t -> (Theory.role Opcode.Map.t) Var.Map.t

(** [freshen_operand o] creates a new operand with the same body and a
    fresh id, if the operation is "var-like" (Var or Void) *)
val freshen_operand : operand -> operand

(** [freshen_operands] gives a new unique tid to every operand. *)
val freshen_operands : t -> t

(** [freshen_operation_ids] gives a unique tid to every operation *)
val freshen_operation_ids : t -> t

(** Various getter functions *)
val operation_opcodes : t -> opcode list Int.Map.t
val all_opcodes : t -> opcode list
val operand_operation : t -> operation Var.Map.t
val op_var_exn : operand -> op_var

(** various printer functions *)
val pretty_operand : operand -> string
val pretty_operation : operation -> string
val pretty_blk : blk -> string
val pretty_ir : t -> string
(* Alias of pretty_ir *)
val to_string : t -> string

(** Populate the [pre_assign] field with [`R0] if it is not already
    assigned. Useful for testing purposes. *)
val dummy_reg_alloc : t -> t

(** Build a mapping from block tid to ins operation *)
val ins_map : t -> operation_id Tid.Map.t

(** Build a mapping from block tid to outs operation *)
val outs_map : t -> operation_id Tid.Map.t

(** Build a mapping from block tid to all operation ids in that block *)
val block_ops : t -> operation_id list Tid.Map.t

