(** Compiles the patch.

    This module is responsible for taking the patch code (BIL statements)
    that was ingested by the {!Patch_ingester}, and "compiling" it to
    assembly (or something like it) for the target architecture. *)

open Bap_knowledge
module KB = Knowledge

(** [compile obj] converts the patch (which is BIL) associated with the
    provided [obj] into assembly. It stores this assembly and the minizinc
    solution into slots of the patches of the [obj]. [compile] also takes
    an optional [solver] parameter for unit testing which defaults
    to [Minizinc.run_minizinc] *)
val compile : ?solver:(string -> Minizinc.sol list -> Ir.t ->
  (Ir.t * Minizinc.sol) KB.t) -> Data.t -> unit KB.t
