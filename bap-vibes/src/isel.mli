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

(*
Contains Minizinc based instruction selector.

Instruction selection is ultimately responsibly for translating from BIR to VIBES IR.
These are roughly analagous to LLVM IR and MLIR, a high level assembly like IR and a more
machine dependent but not completely concrete IR.

It is assumed the BIR is flattened and in (linear?) SSA before being sent to this module.
Flattening the BIR (making the right hand side of a Def a non recursive expression)
simplifies significantly the problem and brings the data struture in correspondence
with the nodes of the Blindell universal instruction selection paper.
Def.t become identified with Blindell's computation nodes.

The data needed by instruction selection is a set of patterns and templates.

Patterns are roughly chunks of BIR that can be found to match the incoming BIR.
Templates are roughly chunks of VIBES IR.

A match is specified by stating what every piece of the pattern corresponds to in
the matchee. All this matching information in excess of just the instantiation of
pattern variables is necessary to make sure the matchee BIR has been completely covered.

Discovering matches is done somewhat naively, via a guess and check procedure.
By tightly interleaving the guessing and checking stages you can create more efficient
versions.

All matches between patterns and matchee are discovered and the covering problem is
sent off to minizinc. It returns which subset of all the possible matches is most desirable.
This minizinc model is a simplification of that described in the Blindell Universal 
instruction selection paper or his thesis.

For instantiating the template, only the matching information for blocks and variables
is needed rather than the ocmplete matching information.
These variables are looked up in the match and substituted into the template.
Other freshening may also be needed in the template to guarantee uniqueness
of certain identifiers of VIBES IR (operand ids, template internal temporaries,
operation ids).

Instantiating the templates does not require information from the minizinc solution. Only
selecting which of the instantiated templates which are to be used.
Because of this, there is promise that one can pre instantiate the templates and build
a combined insturction selection, allocation, and scheduling solver.

It is assumed that the instruction selector is receiving an SSA and flattened BIR
data structure.
*)

open Core_kernel
open Bap.Std
open Bap_knowledge
module KB = Knowledge

(*
  Patterns are the things looked for in the source program
*)
module Pattern : sig
  type t
end

(*
    Every pattern has a template. Templates describe how to take match information from a
    match produced by a Pattern.t and turn it into VIBES IR. 
*)
module Template : sig
  type t
end

type info = (Pattern.t * Template.t) String.Map.t

module Utils : sig
  val binop : binop -> Ir.Opcode.t -> Pattern.t * Template.t
  val store : Ir.Opcode.t -> Pattern.t * Template.t
  val load : Ir.Opcode.t -> Pattern.t * Template.t
  val goto : Ir.Opcode.t -> Pattern.t * Template.t
  val null_jump : Ir.Opcode.t -> Pattern.t * Template.t
  val mov : Ir.Opcode.t -> Pattern.t * Template.t
  val def_pat : Def.t -> Pattern.t
  val x : Var.t
  val z : Var.t
end

val run :
  isel_model_filepath:string ->
  Blk.t list ->
  info ->
  Ir.t KB.t
