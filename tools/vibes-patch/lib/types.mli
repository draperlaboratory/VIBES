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

open Bap_core_theory

module Asm = Vibes_as.Types.Assembly

(** Abstraction behild the toolchain in order to perform the patch. *)
module type Toolchain = sig

  (** Assembles the program and returns the path to the object file,
      along with the number of bytes of inline data that was discovered. *)
  val assemble : Asm.t -> Theory.language -> (string * int, KB.conflict) result

  (** Obtains the raw binary code of the object file. *)
  val to_binary : string -> (string, KB.conflict) result

end

module type Target_utils = sig

  (** [situate asm ~loc ?jmp] will situate the patch program [asm] at the
      location [loc]. [jmp] is the location to jump to at the end of the
      program. [overwritten] is the list of assembly instructions that were
      overwritten, and shall be included in the patch code if [asm] cannot
      erase these instructions. *)
  val situate :
    ?org:int64 option ->
    ?jmp:int64 option ->
    ?overwritten:string list ->
    Asm.t ->
    loc:int64 ->
    to_addr:(int64 -> int64) ->
    Asm.t

  (** [create_trampoline addr patch_point patch_size] creates a fresh
      assembly program for the purpose of jumping to the address [addr]. *)
  val create_trampoline : int64 -> int64 -> int64 -> Asm.t

  (** Returns [true] if the assembly program is expected to
      have inline data when assembled. *)
  val has_inline_data : Asm.t -> bool

  (** Returns [true] if the assembly program ends in an unconditional
      jump. *)
  val ends_in_jump : Asm.t -> bool

  (** Returns the adjusted origin of the patch based on the patch
      location. *)
  val adjusted_org : int64 -> int64 option

  (** The maximum number of bytes that an instruction can occupy. *)
  val max_insn_length : int

  (** The toolchain for the target. *)
  module Toolchain : Toolchain

end

(** Target-specific functionality for performing the patch. *)
module type Target = sig

  (** The target. *)
  val target : Theory.target

  include Target_utils

end
