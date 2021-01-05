(* Creates the patched executable.

   This module is responsible for taking the assembly-like instructions
   produced by the {!Compiler}, converting it into binary instructions,
   and then splicing that binary code into (a copy of) the original
   executable, thereby producing a new, patched executable. *)

open Bap_knowledge
module KB = Knowledge

(* [patch ~patcher obj] uses the [patcher] function to patch the original
   executable associated with the provided [obj].

   The [patcher] can be any function that takes a filepath (a [string]),
   a patch point (an address [Bitvec.t] in the original executable),
   and a list of assembly instructions (a [string list]), and returns
   a filepath (a [string]) to the patched executable. *)
val patch : ?patcher:(string -> string list -> Bitvec.t -> string KB.t) ->
  Data.t -> unit KB.t
