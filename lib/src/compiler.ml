(* Implements {!Compiler}. *)

open !Core_kernel
open Bap_knowledge
open Knowledge.Syntax
open Bap_core_theory

module KB = Knowledge


(* Converts a list of BIL statements to a list of ARM assembly strings.
   This is just a dummy stand-in for now. It only handles a simple move
   instruction. *)
let create_assembly ?solver:(solver = Minizinc.run_minizinc)
    (bir : Theory.Program.t) : string list KB.t =
  let value_exn x = Option.value_exn x in
  let sem = KB.Value.get Theory.Semantics.slot bir in
  Arm_gen.effect sem |>
  value_exn |>
  Arm_gen.ir |>
  solver  >>|
  Arm_gen.arm_ir_pretty >>=
  (function
    | Ok assembly -> KB.return assembly
    | Error e -> Errors.fail e)

(* Converts the patch (as BIL) to assembly instructions. *)
let compile ?solver:(solver = Minizinc.run_minizinc)(obj : Data.t) : unit KB.t =
  Events.(send @@ Header "Starting compiler");
  Events.(send @@ Info "This stage is currently a naive stand-in");

  (* Retrieve the patch (BIL) from the KB, and convert it to assembly. *)
  Events.(send @@ Info "Retreiving data from KB...");
  Data.Patch.get_bir obj >>= fun bir ->
  Events.(send @@ Info "Translating patch BIL to assembly...");
  create_assembly ~solver bir >>= fun assembly ->

  (* Stash the assembly in the KB. *)
  Data.Patch.set_assembly obj (Some assembly) >>= fun _ ->

  Events.(send @@ Info "Done. The patch has the following assembly:");
  Events.(send @@ Rule);
  Events.(send @@ Info (String.concat ~sep:"\n" assembly));
  Events.(send @@ Rule);

  KB.return ()
