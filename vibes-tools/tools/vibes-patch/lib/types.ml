open Bap_core_theory

module Asm = Vibes_as.Types.Assembly

module type Toolchain = sig

  val assemble : Asm.t -> Theory.language -> (string, KB.conflict) result
  val to_binary : string -> (string, KB.conflict) result

end

module type Target_utils = sig

  val situate :
    ?org:int64 option ->
    ?jmp:int64 option ->
    ?overwritten:string list ->
    Asm.t ->
    loc:int64 ->
    to_addr:(int64 -> int64) ->
    Asm.t

  val create_trampoline : int64 -> int64 -> int64 -> Asm.t
  val has_inline_data : Asm.t -> bool
  val ends_in_jump : Asm.t -> bool
  val adjusted_org : int64 -> int64 option
  val max_insn_length : int

  module Toolchain : Toolchain

end

module type Target = sig

  val target : Theory.target

  include Target_utils

end
