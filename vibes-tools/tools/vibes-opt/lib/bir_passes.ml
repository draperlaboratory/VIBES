open Core
open Bap.Std
open Bap_core_theory

module T = Theory
module Log = Vibes_log_lib.Stream
module Utils = Vibes_utils_lib
module Function_info = Vibes_function_info_lib.Types
module Hvar = Vibes_higher_vars_lib.Higher_var
module Subst = Vibes_higher_vars_lib.Substituter
module Patch_info = Vibes_patch_info_lib.Types
module Bir_helpers = Vibes_bir_lib.Helpers

open KB.Syntax

let log_sub (sub : sub term) : unit =
  Log.send "New BIR:\n%a" Sub.pp sub

let liftr (r : ('a, KB.Conflict.t) result) : 'a KB.t = match r with
  | Error err -> KB.fail err
  | Ok r -> !!r

(* Provide function info to direct call destinations. *)
let provide_function_info
    (sub : sub term)
    ~(func_info : Function_info.t) : unit KB.t =
  Term.enum blk_t sub |> KB.Seq.iter ~f:(fun blk ->
      Term.enum jmp_t blk |> KB.Seq.iter ~f:(fun jmp ->
          match Jmp.alt jmp with
          | None -> !!()
          | Some alt -> match Jmp.resolve alt with
            | Second _ -> !!()
            | First tid ->
              KB.List.iter func_info.functions ~f:(fun f ->
                  let* aliases = KB.collect T.Label.aliases tid in
                  if Set.mem aliases f.label then begin
                    let s = Tid.to_string tid in
                    let* () = KB.provide T.Label.aliases tid @@
                      Set.add aliases @@ Option.value_exn f.name in
                    let* () = match f.name with
                      | Some name ->
                        Log.send "Providing name %s for tid %s" name s;
                        KB.provide T.Label.name tid f.name
                      | None -> !!() in
                    let* () = match f.addr with
                      | Some addr ->
                        Log.send "Providing addr %a for tid %s"
                          Bitvec.pp addr s;
                        KB.provide T.Label.addr tid f.addr
                      | None -> !!() in
                    Log.send "Marking tid %s as a subroutine" s;
                    KB.provide T.Label.is_subroutine tid @@ Some true
                  end else !!())))

let run
    (sub : sub term)
    ~(target : T.Target.t)
    ~(language : T.Language.t)
    ~(patch_info : Patch_info.t)
    ~(func_info : Function_info.t) : Types.t KB.t =
  let hvars = Patch_info.patch_vars patch_info in
  let sp_align = Patch_info.sp_align patch_info in
  Log.send "Running BIR passes";
  let is_thumb = Utils.Core_theory.is_thumb language in
  let* entry_blk = liftr @@ Bir_helpers.entry_blk sub in
  let entry_tid = Term.tid entry_blk in
  let* () = provide_function_info sub ~func_info in
  Log.send "Inserting new mems at callsites";
  let* argument_tids = Abi.collect_argument_tids sub ~target ~func_info in
  let* sub, mem_argument_tids =
    Abi.insert_new_mems_at_callsites sub ~target in
  log_sub sub;
  Log.send "Removing unreachable BIR";
  let argument_tids = Tid.Set.union argument_tids mem_argument_tids in
  let sub = Shape.remove_unreachable sub @@ Term.tid entry_blk in
  log_sub sub;
  Log.send "Adjusting exits";
  let* sub = Shape.adjust_exits sub in
  log_sub sub;
  Log.send "Spilling hvars and adjusting stack";
  let* Abi.Spill.{sub; hvars; spilled} =
    Abi.Spill.spill_hvars_and_adjust_stack sub
      ~target ~sp_align ~hvars ~entry_blk in
  log_sub sub;
  Log.send "Substituting hvars";
  let* sub = Subst.substitute sub ~target ~hvars ~spilled ~entry_tid in
  log_sub sub;
  Log.send "Re-ordering blocks";
  let sub = Shape.reorder_blks sub in
  log_sub sub;
  Log.send "Applying optimizations in the Opt module";
  let* sub = liftr @@ Opt.apply sub in
  log_sub sub;
  Log.send "Applying BAP optimizations";
  let sub = Bap_opt.run sub in
  log_sub sub;
  let* sub =
    if is_thumb then begin
      Log.send "Relaxing branches for the Thumb target";
      let+ sub = Shape.relax_branches sub ~target ~patch_info in
      log_sub sub; sub
    end else !!sub  in
  Log.send "Re-ordering blocks again";
  let sub = Shape.reorder_blks sub in
  log_sub sub;
  Log.send "Done with BIR passes";
  !!(Types.create ~sub ~argument_tids)
