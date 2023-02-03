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

open Core

module Ir = Vibes_ir.Types

module Cond = struct

  type t = EQ | NE | LE | GT | LT | GE | HI | LO | HS | LS

  let to_string : t -> string = function
    | EQ -> "eq"
    | NE -> "ne"
    | LE -> "le"
    | GT -> "gt"
    | LT -> "lt"
    | GE -> "ge"
    | HI -> "hi"
    | LO -> "lo"
    | HS -> "hs"
    | LS -> "ls"

  let of_string : string -> t option = function
    | "eq" -> Some EQ
    | "ne" -> Some NE
    | "le" -> Some LE
    | "gt" -> Some GT
    | "lt" -> Some LT
    | "ge" -> Some GE
    | "hi" -> Some HI
    | "lo" -> Some LO
    | "hs" -> Some HS
    | "ls" -> Some LS
    | _ -> None

  let opposite : t -> t = function
    | EQ -> NE
    | NE -> EQ
    | LE -> GT
    | GT -> LE
    | LT -> GE
    | GE -> LT
    | HI -> LS
    | LO -> HS
    | HS -> LO
    | LS -> HI

end

type cond = Cond.t

let op ?(cnd : Cond.t option = None) (s : string) : Ir.opcode =
  s ^ Option.value_map cnd ~default:"" ~f:Cond.to_string

(* With Thumb, there are cases where we need to set the flags to get the
   narrow encoding of the instruction (and other cases where this is the
   opposite). *)

let movcc (cnd : Cond.t) : Ir.opcode = op "mov" ~cnd:(Some cnd)

let mov (set_flags : bool) : Ir.opcode =
  op (if set_flags then "movs" else "mov")

let movw : Ir.opcode = op "movw"

let add (set_flags : bool) : Ir.opcode =
  op (if set_flags then "adds" else "add")

let mul : Ir.opcode = op "mul"

let sub (set_flags : bool) : Ir.opcode =
  op (if set_flags then "subs" else "sub")

let neg : Ir.opcode = op "neg"
let mvn : Ir.opcode = op "mvn"
let lsl_ : Ir.opcode = op "lsl"
let lsr_ : Ir.opcode = op "lsr"
let asr_ : Ir.opcode = op "asr"
let and_ : Ir.opcode = op "and"
let orr : Ir.opcode = op "orr"
let eor : Ir.opcode = op "eor"
let ldr : Ir.opcode = op "ldr"
let ldrh : Ir.opcode = op "ldrh"
let ldrb : Ir.opcode = op "ldrb"
let str : Ir.opcode = op "str"
let strh : Ir.opcode = op "strh"
let strb : Ir.opcode = op "strb"
let cmp : Ir.opcode = op "cmp"

let b ?(cnd : Cond.t option = None) () : Ir.opcode =
  op "b" ~cnd

let bl ?(cnd : Cond.t option = None) () : Ir.opcode =
  op "bl" ~cnd
