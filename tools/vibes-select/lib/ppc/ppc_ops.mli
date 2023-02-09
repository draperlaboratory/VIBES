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

open Vibes_ir.Types

module Cond : sig

  type t = EQ | NE | LE | GT | LT | GE

  val to_string : t -> string
  val of_string : string -> t option
  val opposite : t -> t

end

type cond = Cond.t

val add : opcode
val addi : opcode
val and_ : opcode
val andi : opcode
val b : ?cnd:cond option -> unit -> opcode
val bl : opcode
val cmplw : opcode
val cmplwi : opcode
val cmpw : opcode
val cmpwi : opcode
val lbz : opcode
val lbzx : opcode
val lha : opcode
val lhax : opcode
val lhz : opcode
val lhzx : opcode
val lwz : opcode
val lwzx : opcode
val li : opcode
val lis : opcode
val mfcr : opcode
val mr : opcode
val mulli : opcode
val mullw : opcode
val neg : opcode
val not : opcode
val or_ : opcode
val ori : opcode
val slw : opcode
val slwi : opcode
val sraw : opcode
val srawi : opcode
val srw : opcode
val srwi : opcode
val stb : opcode
val stbx : opcode
val sth : opcode
val sthx : opcode
val stw : opcode
val stwx : opcode
val sub : opcode
val subi : opcode
val xor : opcode
val xori : opcode
