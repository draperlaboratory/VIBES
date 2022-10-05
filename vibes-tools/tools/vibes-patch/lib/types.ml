open Bap_core_theory

module Asm = Vibes_as.Types.Assembly

module type Toolchain = sig

  val assemble : Asm.t -> Theory.language -> (string, KB.conflict) result
  val to_binary : string -> (string, KB.conflict) result

end

module type Target = sig

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
  val adjusted_org : int64 -> int64 option

  module Toolchain : Toolchain

end
