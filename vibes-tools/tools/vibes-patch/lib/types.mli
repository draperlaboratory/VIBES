open Bap_core_theory

module Asm = Vibes_as.Types.Assembly

(** Abstraction behild the toolchain in order to perform the patch. *)
module type Toolchain = sig

  (** Assembles the program and returns the path to the object file. *)
  val assemble : Asm.t -> Theory.language -> (string, KB.conflict) result

  (** Obtains the raw binary code of the object file. *)
  val to_binary : string -> (string, KB.conflict) result

end

(** Target-specific functionality for performing the patch. *)
module type Target = sig

  (** [situate asm ~loc ?jmp] will situate the patch program [asm] at the
      location [loc]. [jmp] is the location to jump to at the end of the
      program. *)
  val situate :
    ?org:int64 option ->
    ?jmp:int64 option ->
    Asm.t ->
    loc:int64 ->
    to_addr:(int64 -> int64) ->
    Asm.t

  (** [create_trampoline loc] creates a fresh assembly program for the
      purpose of jumping to the patch location [loc]. *)
  val create_trampoline : int64 -> Asm.t

  (** Returns [true] if the assembly program is expected to
      have inline data when assembled. *)
  val has_inline_data : Asm.t -> bool

  (** Returns the adjusted origin of the patch based on the patch
      location. *)
  val adjusted_org : int64 -> int64 option
  
  (** The toolchain for the target. *)
  module Toolchain : Toolchain

end
