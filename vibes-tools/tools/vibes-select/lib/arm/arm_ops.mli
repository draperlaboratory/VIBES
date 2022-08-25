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
val cmp : opcode
val b : ?cnd:cond option -> unit -> opcode
val bl : ?cnd:cond option -> unit -> opcode
