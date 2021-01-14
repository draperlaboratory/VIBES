(* Implements {!Compiler}. *)

open !Core_kernel
open Bap.Std
open Bap_knowledge
open Knowledge.Syntax

module KB = Knowledge


(* Converts a list of BIL statements to a list of ARM assembly strings. *)
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

(* Compile one patch from BIL to assembly *)
let compile_one (solver : Vibes_ir.t -> Vibes_ir.t KB.t)
      (count : int KB.t) (patch : Data.Patch.t) : int KB.t =
    count >>= fun n ->
    Events.(send @@ Info (  "Translating patch " ^ string_of_int n
                          ^ " BIL to assembly..."));
    Events.(send @@ Rule);
    Data.Patch.get_bil patch >>= fun bil ->
    create_assembly ~solver bil >>= fun assembly ->

    (* Stash the assembly in the KB. *)
    Data.Patch.set_assembly patch (Some assembly) >>= fun () ->
    Events.(send @@ Info "The patch has the following assembly:\n");
    Events.(send @@ Info (String.concat ~sep:"\n" assembly));
    Events.(send @@ Rule);

    KB.return (n+1)

(* Converts all patches (as BIL) to assembly instructions. *)
let compile ?solver:(solver = Minizinc.run_minizinc)(obj : Data.t) : unit KB.t =
  Events.(send @@ Header "Starting compiler");

  (* Retrieve the patch (BIL) from the KB, and convert it to assembly. *)
  Events.(send @@ Info "Retreiving data from KB...");
  Data.Patched_exe.get_patches obj >>= fun patches ->
  let size : string = string_of_int (Data.Patch_set.length patches) in
  Events.(send @@ Info ("There are " ^ size ^ " patch fragments."));
  Data.Patch_set.fold patches ~init:(KB.return 1)
    ~f:(compile_one solver) >>= fun _ ->
  Events.(send @@ Info "Done.");
  KB.return ()
