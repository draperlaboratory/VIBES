(*
   This IR is intended for the eventual serialization of parameters to a cponstraint model
   that solves a joint instructions scheduling and register allocation problem in the style
   of Unison. It is modeled roughly after the Unison IR as described in chapter 5 of
   the Unison manual.

   Because the IR needs to be serialized to an external solver, many entities require ids.

   Unison makes the distinction between Operands and Temporaries.
   And operand is a field of an operation. One operand may have multiple temporaries available
   from which to choose from. This conceptual separation opens up enough space to model
   spilling registers and live range splitting.
   An operand may be preassigned to a particular register.

   Unison also makes a distinction between and Operation and an Instruction.
   An operation may be implemented via different instructions.
   Instructions correspond typically to assembly operators.
   As a simplification, it can be useful to consider

   It is intended that this IR be put into linear SSA form in which
   on top of SSA, temporaries belong uniquely to a single block.
   Temporaries that persist across blocks are recorded as congruent in the [vibes_ir] type.
   This can be achived by namespacing variables by the block they belong to.
   The purpose of the linear SSA is to helpthe constraint satisfaction problem conceputally
   decompose into coupled block level constraint satisfaction problems.

*)

open Bap.Std


(* [operand]s are named, possibly optional, have a list of potential
   temporaries that can be used to implement the operand and may be
   pre-assigned to registers for calling conventions or other reasons
   *)
type op_var = {
  id : var;
  temps : var list;
  pre_assign : var option
} [@@deriving compare, equal]

val simple_var : var -> op_var

type operand = Var of op_var | Const of word [@@deriving compare, equal]

(** An [operation] has an id
    an assigned lhs,
    a set of instructions to choose from,
    a flag of whether the operation is optional,
    a list of operands *)
type operation = {
  id : tid;
  lhs : operand;
  insns : ARM.insn list;
  optional : bool;
  operands : operand list;
} [@@deriving compare, equal]

val simple_op : ARM.insn -> operand -> operand list -> operation

(** A [vibes_blk] has an id,
    a set of operations,
    a set of input temporaries assumed to exists at the beginning of the block,
    a set of output temporaries assumed to persist out the end of the block,
    an estimated execution frequency.
*)
type blk = {
  id : tid;
  operations : operation list;
  ins : Var.Set.t;
  outs : Var.Set.t;
  frequency : int
} [@@deriving compare, equal]

(** Create a block given a [tid] and a list of [operation]s, filling
   in default values for the other fields. *)
val simple_blk : tid -> operation list -> blk

(**
   The [vibes_ir] type has a list of blocks and a set of operands which are congruent.
*)
type t = {
  blks : blk list;
  congruent : (operand * operand) list
} [@@deriving compare, equal]

val empty : t

val union : t -> t -> t

val add : blk -> t -> t
