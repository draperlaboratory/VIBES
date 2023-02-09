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

  type t = EQ | NE | LE | GT | LT | GE

  let to_string : t -> string = function
    | EQ -> "eq"
    | NE -> "ne"
    | LE -> "le"
    | GT -> "gt"
    | LT -> "lt"
    | GE -> "ge"

  let of_string : string -> t option = function
    | "eq" -> Some EQ
    | "ne" -> Some NE
    | "le" -> Some LE
    | "gt" -> Some GT
    | "lt" -> Some LT
    | "ge" -> Some GE
    | _ -> None

  let opposite : t -> t = function
    | EQ -> NE
    | NE -> EQ
    | LE -> GT
    | GT -> LE
    | LT -> GE
    | GE -> LT

end

type cond = Cond.t

let op ?(cnd : Cond.t option = None) (s : string) : Ir.opcode =
  s ^ Option.value_map cnd ~default:"" ~f:Cond.to_string

let add : Ir.opcode = op "add"
let addi : Ir.opcode = op "addi"
let and_ : Ir.opcode = op "and"
let andi : Ir.opcode = op "andi." (* note the dot, this updates CR0 *)
let b ?(cnd : cond option = None) () : Ir.opcode = op "b" ~cnd
let bl : Ir.opcode = op "bl"
let cmplw : Ir.opcode = op "cmplw"
let cmplwi : Ir.opcode = op "cmplwi"
let cmpw : Ir.opcode = op "cmpw"
let cmpwi : Ir.opcode = op "cmpwi"
let lbz : Ir.opcode = op "lbz"
let lbzx : Ir.opcode = op "lbzx"
let lha : Ir.opcode = op "lha"
let lhax : Ir.opcode = op "lhax"
let lhz : Ir.opcode = op "lhz"
let lhzx : Ir.opcode = op "lhzx"
let lwz : Ir.opcode = op "lwz"
let lwzx : Ir.opcode = op "lwzx"
let li : Ir.opcode = op "li"
let lis : Ir.opcode = op "lis"
let mfcr : Ir.opcode = op "mfcr"
let mr : Ir.opcode = op "mr"
let mulli : Ir.opcode = op "mulli"
let mullw : Ir.opcode = op "mullw"
let neg : Ir.opcode = op "neg"
let not : Ir.opcode = op "not"
let or_ : Ir.opcode = op "or"
let ori : Ir.opcode = op "ori"
let slw : Ir.opcode = op "slw"
let slwi : Ir.opcode = op "slwi"
let sraw : Ir.opcode = op "sraw"
let srawi : Ir.opcode = op "srawi"
let srw : Ir.opcode = op "srw"
let srwi : Ir.opcode = op "srwi"
let stb : Ir.opcode = op "stb"
let stbx : Ir.opcode = op "stbx"
let sth : Ir.opcode = op "sth"
let sthx : Ir.opcode = op "sthx"
let stw : Ir.opcode = op "stw"
let stwx : Ir.opcode = op "stwx"
let sub : Ir.opcode = op "sub"
let subi : Ir.opcode = op "subi"
let xor : Ir.opcode = op "xor"
let xori : Ir.opcode = op "xori"
