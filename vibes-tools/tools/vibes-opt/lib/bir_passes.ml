open Core
open Bap.Std
open Bap_core_theory

module T = Theory
module Log = Vibes_log.Stream
module Utils = Vibes_utils
module Hvar = Vibes_higher_vars.Higher_var
module Subst = Vibes_higher_vars.Substituter
module Patch_info = Vibes_patch_info.Types
module Bir_helpers = Vibes_bir.Helpers
module Spaces = Patch_info.Spaces
module Tags = Vibes_bir.Tags

open KB.Syntax

let log_sub (sub : sub term) : unit =
  Log.send "New BIR:\n\n%a" Sub.pp sub

let liftr (r : ('a, KB.conflict) result) : 'a KB.t = match r with
  | Error err -> KB.fail err
  | Ok r -> !!r

let provide_if_none
    (slot : ('a, 'b option) KB.slot)
    (obj : 'a KB.obj)
    (x : 'b) : unit KB.t =
  let* existing = KB.collect slot obj in
  match existing with
  | None -> KB.provide slot obj @@ Some x
  | Some _ -> !!()

(* Provide function info to direct call destinations.

   This function info is needed if we are loading the serialized program
   from disk, or if we're interacting with the program from a fresh
   Knowledge Base.

   If we're operating from the output of the [Core_c] pass, then the
   corresponding slots of each relevant program label will already be
   populated with this info, so if we're providing it again then it
   must be consistent with the information already in the KB, otherwise
   we will get a conflict.
*)
let provide_function_info (sub : sub term) : unit KB.t =
  Term.enum blk_t sub |> KB.Seq.iter ~f:(fun blk ->
      Term.enum jmp_t blk |> KB.Seq.iter ~f:(fun jmp ->
          match Jmp.alt jmp with
          | None -> !!()
          | Some alt -> match Jmp.resolve alt with
            | Second _ -> !!()
            | First alt ->
              let* () = match Term.get_attr jmp Tags.name_dest with
                | Some name -> provide_if_none T.Label.name alt name
                | None -> !!() in
              let* () = match Term.get_attr jmp Tags.addr_dest with
                | Some addr -> provide_if_none T.Label.addr alt addr
                | None -> !!() in
              KB.provide T.Label.is_subroutine alt @@ Some true))

let thumb_specific
    ?(patch_spaces : Spaces.t = Spaces.empty)
    (sub : sub term)
    ~(target : T.target)
    ~(patch_info : Patch_info.t) : sub term KB.t =
  Log.send "Relaxing branches";
  let* sub = Shape.relax_branches sub
      ~target ~patch_info ~patch_spaces
      ~fwd_limit:0xFFFFE
      ~bwd_limit:0x100000 in
  log_sub sub;
  Log.send "Splitting conditional jumps";
  let+ sub = Shape.split_on_conditional sub in
  log_sub sub;
  sub

let run
    ?(patch_spaces : Spaces.t = Spaces.empty)
    (sub : sub term)
    ~(target : T.target)
    ~(language : T.language)
    ~(patch_info : Patch_info.t) : sub term KB.t =
  let hvars = patch_info.patch_vars in
  let sp_align = patch_info.sp_align in
  Log.send "Running BIR passes";
  let is_thumb = Utils.Core_theory.is_thumb language in
  let* () = provide_function_info sub in
  Log.send "Inserting new mems at callsites";
  let* sub = Abi.insert_new_mems_at_callsites sub ~target in
  log_sub sub;
  Log.send "Removing unreachable BIR";
  let* sub = liftr @@ Shape.remove_unreachable sub in
  log_sub sub;
  Log.send "Adjusting exits";
  let* sub = Shape.adjust_exits sub in
  log_sub sub;
  Log.send "Splitting conditional calls";
  let* sub = Shape.split_conditional_calls sub in
  log_sub sub;
  Log.send "Spilling hvars and adjusting stack";
  let* sub, hvars =
    Abi.spill_hvars_and_adjust_stack sub ~target ~sp_align ~hvars in
  log_sub sub;
  Log.send "Substituting hvars";
  let* sub = Subst.substitute sub ~target ~hvars in
  log_sub sub;
  Log.send "Re-ordering blocks";
  let sub = Shape.reorder_blks sub in
  log_sub sub;
  Log.send "Applying optimizations in the Opt module";
  let* sub = Opt.apply hvars sub in
  log_sub sub;
  let* sub =
    if is_thumb then begin
      Log.send "%a target detected" T.Language.pp language;
      thumb_specific sub ~target ~patch_info ~patch_spaces
    end else !!sub in
  Log.send "Re-ordering blocks again";
  let sub = Shape.reorder_blks sub in
  log_sub sub;
  Log.send "Done with BIR passes";
  !!sub
