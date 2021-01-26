(* Compiles the patch.

   This module is responsible for taking the patch code (BIL statements)
   that was ingested by the {!Patch_ingester}, and "compiling" it to
   assembly (or something like it) for the target architecture. *)

open Bap_knowledge
module KB = Knowledge

(* [compile obj] converts the patch (which is BIL) associated with the
   provided [obj] into assembly. *)
val compile : ?solver:(Vibes_ir.t -> Vibes_ir.t KB.t) -> Data.t -> unit KB.t
