(** Compiles the patch.

    This module is responsible for taking the patch code (BIL statements)
    that was ingested by the {!Patch_ingester}, and "compiling" it to
    assembly (or something like it) for the target architecture. *)

open Bap_core_theory

(** [compile_ir obj] converts the patch (which is BIL) associated with the
    provided [obj] into VIBES IR. It stores this IR
    into slots of the patches of the [obj]. *)
    val compile_ir : ?isel_model_filepath:string -> Data.t -> unit KB.t

(** [compile_assembly obj] converts the patch IR associated with the
    provided [obj] into assembly. It stores this assembly and the minizinc
    solution into slots of the patches of the [obj]. [compile_assembly] also takes
    an optional [solver] parameter for unit testing which defaults
    to [Minizinc.run_minizinc] *)
val compile_assembly :
  ?solver:(?exclude_regs:Core_kernel.String.Set.t ->
           Theory.target ->
           Theory.language ->
           filepath:string ->
           Minizinc.sol list ->
           Ir.t ->
           (Ir.t * Minizinc.sol) KB.t) ->
  Data.t ->
  unit KB.t


