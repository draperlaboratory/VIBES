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

open Core_kernel
open Bap.Std
open Bap_core_theory

(** Use BAP's definition of immediate sizes. *)
type nonrec size = size

(** Subset of `Cabs.sign`, where now signedness is explicit. *)
type sign = SIGNED | UNSIGNED

(** Subset of `Cabs.base_type` for types supported by VIBES. *)
type typ =
  | VOID
  | INT of size * sign
  | PTR of typ
  | FUN of typ * typ list

(** Returns the size of the type in bits. *)
val size_of_typ : Theory.target -> typ -> int

(** Returns the signedness of the type. *)
val sign_of_typ : typ -> sign

(** Compares two types for equality. *)
val equal_typ : typ -> typ -> bool

(** Subset of `Cabs.binary_operator`, where only pure binary operations
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

(** Subset of `Cabs.unary_operator`, where only pure unary operations
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

(** Subset of `Cabs.expression`, where all expressions are pure and
    explicitly typed. *)
type exp =
  | UNARY of unop * exp * typ
  | BINARY of binop * exp * exp * typ
  | CAST of typ * exp
  | CONST_INT of word * sign
  | VARIABLE of var

(** Subset of `Cabs.statement`. *)
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

(** A PatchC definition is a scoped statement. *)
type t = body

val equal_exp : exp -> exp -> bool
val equal_stmt : stmt -> stmt -> bool

val string_of_exp : exp -> string
val string_of_stmt : stmt -> string

(** Returns the type embedded in an expression. *)
val typeof : exp -> typ

(** Pretty prints the PatchC definition. *)
val to_string : t -> string

(** Translate a FrontC definition to a PatchC definition. *)
val translate : Cabs.definition -> target:Theory.target -> t KB.t
