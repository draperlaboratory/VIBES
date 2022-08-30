open Core
open Bap.Std
open Bap_core_theory

type opcode = string [@@deriving compare, equal, sexp]

type 'a opcode_map = 'a String.Map.t [@@deriving compare, equal, sexp]

type id = int [@@deriving compare, equal, sexp]

type 'a id_map = 'a Int.Map.t [@@deriving compare, equal, sexp]

type id_set = Int.Set.t [@@deriving compare, equal, sexp]

module Roles : sig

  type map = Theory.role opcode_map id_map

  val dummy : Theory.role
  val preassigned : Theory.role

end

val fresh_id : unit -> id

module Opvar : sig

  type t = {
    id : id;
    temps : var list;
    preassign : var option;
  } [@@deriving compare]

  val create : ?preassign:var option -> var -> t
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit

end

module Operand : sig

  type t =
    | Var of Opvar.t
    | Const of Word.t
    | Label of Tid.t
    | Void of Opvar.t
    | Offset of Word.t
  [@@deriving compare, equal]

  val freshen : t -> t
  val var_operands : t -> Opvar.t list
  val pp : Format.formatter -> t -> unit

end

module Operation : sig

  type t = {
    id : id;
    lhs : Operand.t list;
    opcodes : opcode list;
    optional : bool;
    operands : Operand.t list;
  } [@@deriving compare, equal]

  val create_empty : unit -> t

  val create_simple :
    ?optional:bool ->
    opcode ->
    Operand.t ->
    Operand.t list ->
    t

  val create_multi_lhs :
    ?optional:bool ->
    opcode ->
    Operand.t list ->
    Operand.t list ->
    t

  val create_void : ?optional:bool -> opcode -> t

  val all_operands : t -> Operand.t list
  val freshen : t -> t
  val pp : Format.formatter -> t -> unit

end

module Block : sig

  type t = {
    tid : Tid.t;
    data : Operation.t list;
    ctrl : Operation.t list;
    ins : Operation.t;
    outs : Operation.t;
    frequency : int;
  } [@@deriving compare, equal]

  val create_simple :
    ?frequency:int ->
    tid ->
    data:Operation.t list ->
    ctrl:Operation.t list ->
    t

  val all_operands : t -> Operand.t list
  val all_lhs_operands : t -> Operand.t list
  val all_rhs_operands : t -> Operand.t list
  val all_temps : t -> Var.Set.t
  val definer_map : t -> Opvar.t Var.Map.t
  val users_map : t -> Opvar.t list Var.Map.t
  val all_operations : t -> Operation.t list
  val operation_to_opcodes : t -> opcode list id_map
  val operand_to_operation : t -> Operation.t id_map
  val pp : Format.formatter -> t -> unit

end

type t = {
  blks : Block.t list;
  congruences : Var.Set.t Var.Map.t;
} [@@deriving compare, equal]

val empty : t
val union : t -> t -> t

val add : Block.t -> t -> t

val map_blks : t -> f:(Block.t -> Block.t) -> t
val map_operations : t -> f:(Operation.t -> Operation.t) -> t
val map_opvars : t -> f:(Opvar.t -> Opvar.t) -> t

val freshen_operands : t -> t
val freshen_operation_ids : t -> t

val all_temps : t -> Var.Set.t
val all_opvar_ids : t -> id_set
val all_opcodes : t -> opcode list

val definer_map : t -> Opvar.t Var.Map.t                         
val users_map : t -> Opvar.t list Var.Map.t

val opvar_to_preassign : t -> var id_map
val temp_to_block : t -> tid Var.Map.t
val operation_to_opcodes : t -> opcode list id_map
val operand_to_operation : t -> Operation.t id_map

val op_classes : t -> Roles.map

val block_to_ins : t -> id Tid.Map.t
val block_to_outs : t -> id Tid.Map.t
val block_to_operations : t -> id list Tid.Map.t

val populate_ins_outs : t -> (Var.Set.t * Var.Set.t) Tid.Map.t -> t

val pp : Format.formatter -> t -> unit
