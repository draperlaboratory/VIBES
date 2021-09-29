(* Implements {!Patch_ingester}. *)

open Bap.Std
open Bap_knowledge
open Knowledge.Syntax
open Core_kernel
open Bap_core_theory

module KB = Knowledge
open KB.Let

let provide_bir (tgt : Theory.target) (patch : Data.Patch.t) : unit KB.t =
  Theory.instance () >>=
  Theory.require >>= fun (module Core) ->
  let module CParser = Core_c.Eval(Core) in
  Data.Patch.init_sem patch >>= fun () ->
  Data.Patch.get_patch_name_exn patch >>= fun name ->
  Data.Patch.get_patch_code_exn patch >>= fun code ->
  Events.(send @@ Info (Printf.sprintf "Patch %s" name));
  let code_str = Utils.print_c Cprint.print_def code in
  Events.(send @@ Info (Printf.sprintf "%s" code_str));

  (* Get the patch (as BIR). *)
  let* bir = CParser.c_patch_to_eff tgt code in

  Events.(send @@ Info "The patch has the following BIL:");
  Events.(send @@ Rule);
  let bir_str = Format.asprintf "%a" Bil.pp (KB.Value.get Bil.slot bir) in
  Events.(send @@ Info bir_str);
  Events.(send @@ Rule);
  Data.Patch.set_bir patch bir

(* Ingests a single patch, populating the relevant fields of the KB,
   most notably the semantics field of the corresponding patch (and
   increments the [patch_num] counter). *)
let ingest_one (tgt : Theory.target) (patch_num : int KB.t) (patch : Data.Patch.t)
    : int KB.t =
  patch_num >>= fun patch_num ->
  Events.(send @@ Info (Printf.sprintf "\nIngesting patch %d." patch_num));
  (Data.Patch.get_assembly patch >>= fun asm ->
  match asm with
  | Some _asm -> KB.return () (* Assembly is user provided *)
  | None -> provide_bir tgt patch) >>= fun () ->
  KB.return @@ patch_num+1

(* Processes the whole patch associated with [obj], populating all the
   relevant KB slots with semantic data associated with the patch
   syntax. *)
let ingest (obj : Data.t) : unit KB.t =
  Events.(send @@ Header "Starting patch ingester");

  Events.(send @@ Info "Retreiving data from KB...");
  Data.Original_exe.get_target_exn obj >>= fun tgt ->
  Data.Patched_exe.get_patches obj >>= fun patches ->
  Events.(send @@ Info (Printf.sprintf "There are %d patches"
                          (Data.Patch_set.length patches)));

  Data.Patch_set.fold patches
    ~init:(KB.return 1)
    ~f:(ingest_one tgt) >>= fun _ ->

  Events.(send @@ Info "Patch ingest complete");
  KB.return ()
