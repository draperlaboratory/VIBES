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

  type t = EQ | NE | LE | GT | LT | GE | HI | LO | HS | LS

  val to_string : t -> string
  val of_string : string -> t option
  val opposite : t -> t
  
end

type cond = Cond.t

val movcc : cond -> opcode
val mov : bool -> opcode
val movw : opcode
val add : bool -> opcode
val mul : opcode
val sub : bool -> opcode
val neg : opcode
val mvn : opcode
val lsl_ : opcode
val lsr_ : opcode
val asr_ : opcode
val and_ : opcode
val orr : opcode
val eor : opcode
val ldr : opcode
val ldrh : opcode
val ldrb : opcode
val str : opcode
val strh : opcode
val strb : opcode
val cmp : opcode
val b : ?cnd:cond option -> unit -> opcode
val bl : ?cnd:cond option -> unit -> opcode
