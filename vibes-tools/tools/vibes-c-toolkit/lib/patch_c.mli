(** This module implements PatchC, which is a subset of the FrontC abstract
    representation of C programs. 

    Similar to other intermediate languages like CIL, AST nodes are elaborated
    and explicitly typed. Through elaboration, we simplify the AST and
    separate expressions from statements in that only statements may produce
    side effects. This combination of design choices makes PatchC a more
    adequate intermediate language for lowering to Core Theory.

    In addition, the supported subset of C language features are those suited
    for writing patches, as opposed to full C programs. Hence the name
    "PatchC".
*)

open Core
open Bap.Std
open Bap_c.Std
open Bap_core_theory

module Data_model : sig

  (** The C data model. *)
  type t

  (** Returns the data model for integer types. *)
  val sizes : t -> C.Data.model

  (** Returns true if `char` is signed by default. *)
  val schar : t -> bool

end

(** Use BAP's definition of immediate sizes. *)
type size = [`r8 | `r16 | `r32 | `r64]

(** Subset of [Cabs.sign], where now signedness is explicit. *)
type sign = SIGNED | UNSIGNED

(** Same as BAP C's types. Not all are currently supported by
    the compiler. *)
type typ = C.Type.t

(** Compares two types for equality. *)
val equal_typ : typ -> typ -> bool

module Type : sig

  type t = typ

  val equal : t -> t -> bool

  (** Returns the signedness of the type. *)
  val sign : Data_model.t -> typ -> sign

end

(** Subset of [Cabs.binary_operator], where only pure binary operations
    are allowed. *)
type binop =
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | LAND
  | LOR
  | XOR
  | SHL
  | SHR
  | EQ
  | NE
  | LT
  | GT
  | LE
  | GE

(** Subset of [Cabs.unary_operator], where only pure unary operations
    are allowed. *)
type unop =
  | MINUS
  | LNOT
  | MEMOF
  | ADDROF

(** The typing environment. *)
type tenv = typ String.Map.t

(** A typed var. *)
type var = Theory.Var.Top.t * typ

(** Subset of [Cabs.expression], where all expressions are pure and
    explicitly typed. *)
type exp =
  | UNARY of unop * exp * typ
  | BINARY of binop * exp * exp * typ
  | CAST of typ * exp
  | CONST_INT of word * sign
  | VARIABLE of var

(** Subset of [Cabs.statement]. 

    [STORE (l, r)] takes two expressions as [l] and [r].
    [l] evaluates to a pointer to some location in memory, whose
    contents are replaced with [r]. It is equivalent to [*l = r]
    in normal C parlance.

    It is distinct from [ASSIGN (v, e)], where we are explicitly
    updating the contents of a variable [v] with [e]. Since the
    storage classifications for variables (e.g. stack, global, or
    register) are not determined by PatchC, we assume that variables
    and memory locations are distinct. Therefore, we require a
    distinction between writing to a memory location and assigning
    to a variable.

    [CALLASSIGN (v, f, args)] explicitly says that the return value
    of calling [f] with [args] will be assigned to [v]. We cannot
    use [ASSIGN (v, CALL (f, args))], since [CALL] is not a pure
    expression, and all expressions in PatchC must be pure.
*)
and stmt =
  | NOP
  | BLOCK of body
  | ASSIGN of var * exp
  | CALL of exp * exp list
  | CALLASSIGN of var * exp * exp list
  | STORE of exp * exp
  | SEQUENCE of stmt * stmt
  | IF of exp * stmt * stmt
  | GOTO of string

(** A scope where statements may occur under a typing environment. *)
and body = tenv * stmt

(** A PatchC definition is a scoped statement. We also include the
    data model for sizing of integers. *)
type t = {
  data  : Data_model.t;
  csize : C.Size.base;
  body  : body;
}

module Exp : sig

  type t = exp [@@deriving equal]

  val to_string : t -> string

  (** Returns the type embedded in an expression. *)
  val typeof : Data_model.t -> t -> typ

  (** Force an expression to carry a new type. This operation is
      unsafe unless you know what you're doing! *)
  val coerce_type : Data_model.t -> C.Size.base -> t -> typ -> t

end

module Stmt : sig

  type t = stmt [@@deriving equal]

  val to_string : t -> string

end

(** Pretty prints the PatchC definition. *)
val to_string : t -> string

(** Translate a FrontC definition to a PatchC definition. *)
val translate : Cabs.definition -> target:Theory.target -> t KB.t
