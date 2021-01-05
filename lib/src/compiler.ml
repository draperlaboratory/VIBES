(* Implements {!Compiler}. *)

open !Core_kernel
open Bap.Std
open Bap_knowledge
open Knowledge.Syntax

module KB = Knowledge


(* Converts a list of BIL statements to a list of ARM assembly strings.
   This is just a dummy stand-in for now. It only handles a simple move
   instruction. *)
let create_assembly ?solver:(solver = Minizinc.run_minizinc) (bil : Bil.t) : string list KB.t =
  let value_exn x = Option.value_exn x in
  Arm_gen.BilARM.run Arm_gen.bil_to_arm bil >>|
  (fun v -> v |>
            Arm_gen.effect |>
            value_exn |>
            Arm_gen.ir) >>=
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
  Data.Patch.get_bil obj >>= fun bil ->
  Events.(send @@ Info "Translating patch BIL to assembly...");
  create_assembly ~solver bil >>= fun assembly ->

  (* Stash the assembly in the KB. *)
  Data.Patch.set_assembly obj (Some assembly) >>= fun _ ->

  Events.(send @@ Info "Done. The patch has the following assembly:");
  Events.(send @@ Rule);
  Events.(send @@ Info (String.concat ~sep:"\n" assembly));
  Events.(send @@ Rule);

  KB.return ()
