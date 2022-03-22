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

(* Implements {!Patch_ingester}. *)

open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

open KB.Let

(* Constructs the semantics for a single patch and stashes it in the KB. *)
let provide_sem (tgt : T.target) (patch : Data.Patch.t) : unit KB.t =
  let* theory = T.instance () in
  let* (module Core) = T.require theory in
  let module CParser = Core_c.Eval(Core) in
  let* () = Data.Patch.init_sem patch in
  let* name = Data.Patch.get_patch_name_exn patch in
  let* code = Data.Patch.get_patch_code_exn patch in
  Events.(send @@ Info (Printf.sprintf "Patch %s" name));
  let code_str = Utils.print_c Cprint.print_def code in
  Events.(send @@ Info (Printf.sprintf "%s" code_str));

  (* Get the patch (as BIR). *)
  let* hvars = Data.Patch.get_patch_vars_exn patch in
  let* insn = CParser.c_patch_to_eff hvars tgt code in

  Events.(send @@ Info "The patch has the following BIL:");
  Events.(send @@ Rule);
  let bil = Format.asprintf "%a" Bil.pp (KB.Value.get Bil.slot insn) in
  Events.(send @@ Info bil);
  Events.(send @@ Rule);
  Data.Patch.set_sem patch insn

(* Ingests a single patch, populating the relevant fields of the KB,
   most notably the semantics field of the corresponding patch (and
   increments the [patch_num] counter). *)
let ingest_one (tgt : T.target) (patch_num : int KB.t) (patch : Data.Patch.t)
    : int KB.t =
  let* patch_num = patch_num in
  Events.(send @@ Info (Printf.sprintf "\nIngesting patch %d." patch_num));
  let* asm = Data.Patch.get_assembly patch in
  let* () = match asm with
    | Some _asm -> KB.return () (* Assembly is user provided *)
    | None -> provide_sem tgt patch
  in
  KB.return @@ patch_num + 1

(* Processes the whole patch associated with [obj], populating all the
   relevant KB slots with semantic data associated with the patch
   syntax. *)
let ingest (obj : Data.t) : unit KB.t =
  Events.(send @@ Header "Starting patch ingester");

  Events.(send @@ Info "Retreiving data from KB...");
  let* tgt = Data.Original_exe.get_target_exn obj in
  let* patches = Data.Patched_exe.get_patches obj in
  Events.(send @@ Info 
    (Printf.sprintf "There are %d patches" (Data.Patch_set.length patches)));

  let* _ = Data.Patch_set.fold patches
    ~init:(KB.return 1)
    ~f:(ingest_one tgt)
  in

  Events.(send @@ Info "Patch ingest complete");
  KB.return ()
