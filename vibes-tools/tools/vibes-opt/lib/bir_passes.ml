open Core
open Bap.Std

module T = Bap_core_theory.Theory
module Log = Vibes_log_lib.Stream
module Err = Vibes_error_lib.Std
module Utils = Vibes_utils_lib
module Function_info = Vibes_function_info_lib.Types
module Hvar = Vibes_higher_vars_lib.Higher_var
module Subst = Vibes_higher_vars_lib.Substituter
module Patch_info = Vibes_patch_info_lib.Types
module Bir_helpers = Vibes_bir_lib.Helpers

open Vibes_error_lib.Let

let log_bir bir =
  Log.send (Format.asprintf "New BIR:\n%a" Bap.Std.Blk.pp bir)

let run ~(target : T.Target.t) ~(language : T.Language.t) 
    ~(patch_info : Patch_info.t) ~(func_info : Function_info.t)
    ~(hvars : Hvar.t list) ~(sp_align : int) (bir : blk term list)
    : (Types.t, Err.t) result =
  Log.send "Running BIR passes";

  let is_thumb = Utils.Core_theory.is_thumb language in

  let- entry_blk = match bir with
    | blk :: _ -> Ok blk
    | [] ->
       let msg = "Bir_passes: Blk.from_insns returned an empty list of blks" in
       Error (Types.No_blks msg)
  in

  Log.send "Inserting new mems at callsites";
  let argument_tids = Abi.collect_argument_tids bir ~target ~func_info in
  let bir, mem_argument_tids = Abi.insert_new_mems_at_callsites target bir in
  List.iter bir ~f:log_bir;

  Log.send "Removing unreachable BIR";
  let argument_tids = Tid.Set.union argument_tids mem_argument_tids in
  let bir = Shape.remove_unreachable bir @@ Term.tid entry_blk in
  List.iter bir ~f:log_bir;

  Log.send "Adjusting exits";
  let- bir = Shape.adjust_exits bir in
  List.iter bir ~f:log_bir;

  Log.send "Spilling hvars and adjusting stack";
  let- Abi.Spill.{blks; hvars; spilled} =
    Abi.Spill.spill_hvars_and_adjust_stack bir
      ~target ~sp_align ~hvars ~entry_blk in
  List.iter blks ~f:log_bir;

  Log.send "Substituting hvars";
  let- bir = Subst.substitute blks ~tgt:target ~hvars ~spilled
    ~entry_tid:(Term.tid entry_blk) in
  List.iter bir ~f:log_bir;

  Log.send "Re-ordering blocks";
  let bir = Shape.reorder_blks bir in
  List.iter bir ~f:log_bir;

  Log.send "Applying optimizations in the Opt module";
  let- bir = Opt.apply bir in
  List.iter bir ~f:log_bir;

  Log.send "Applying BAP optimizations";
  let sub = Bir_helpers.create_sub bir in
  let sub = Opt.Bap_opt.run sub in
  let bir = Term.enum blk_t sub |> Seq.to_list in
  List.iter bir ~f:log_bir;

  let- bir =
    if is_thumb then
      begin
        Log.send "This is thumb. Relaxing branches";
        let bir = Shape.relax_branches bir ~target ~patch_info in
        List.iter bir ~f:log_bir;
        Ok bir
      end
    else Ok bir in

  let- bir =
    if is_thumb then
      begin
        Log.send "This is thumb. Splitting on conditionals";
        let bir = Shape.split_on_conditional bir in
        Ok bir
      end
    else Ok bir in

  Log.send "Re-ordering blocks again";
  let bir = Shape.reorder_blks bir in
  List.iter bir ~f:log_bir;
 
  Log.send "Getting the new CFG";
  let sub = Bir_helpers.create_sub bir in
  let cfg = Sub.to_graph sub in

  Log.send "Done with BIR passes";
  let result = Types.create bir ~cfg ~argument_tids in
  Ok result
