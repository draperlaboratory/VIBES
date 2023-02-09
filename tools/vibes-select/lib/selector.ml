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

open Core
open Bap.Std
open Bap_core_theory

module T = Theory
module Log = Vibes_log.Stream
module Utils = Vibes_utils
module CT = Utils.Core_theory
module Tags = Vibes_bir.Tags
module Ir = Vibes_ir.Types
module Linear = Vibes_linear_ssa.Linearizer
module Hvar = Vibes_higher_vars.Higher_var

open KB.Syntax

let run
    (sub : sub term)
    ~(hvars : Hvar.t list)
    ~(target : T.target)
    ~(language : T.language) : Ir.t KB.t =
  Log.send "Transforming to Linear SSA form";
  let* sub = Linear.transform sub ~hvars in
  Log.send "Linearized BIR:\n%a" Sub.pp sub;
  let* select, preassign =
    if CT.is_arm32 target then
      let is_thumb = CT.is_thumb language in
      !!(Arm_selector.select ~is_thumb, Arm_utils.preassign ~is_thumb)
    else if CT.is_ppc32 target then
      !!(Ppc_selector.select, fun t v -> Var.create v t)
    else
      let msg = Format.asprintf
          "Unsupported target %a"
          T.Target.pp target in
      KB.fail @@ Errors.Unsupported_target msg in
  Log.send "Running the %a (%a) instruction selector"
    T.Target.pp target T.Language.pp language;
  let+ ir = select sub in
  let ins_outs =
    Term.enum blk_t sub |>
    Seq.fold ~init:Tid.Map.empty ~f:(fun acc blk ->
        let ins = match Term.get_attr blk Tags.ins with
          | None -> Var.Set.empty
          | Some ins -> ins in
        let outs = match Term.get_attr blk Tags.outs with
          | None -> Var.Set.empty
          | Some outs -> outs in
        Map.set acc ~key:(Term.tid blk) ~data:(ins, outs)) in
  let ir_cong = match Term.get_attr sub Tags.congruences with
    | Some congruences -> Ir.{empty with congruences}
    | None -> Ir.empty in
  let ir = Ir.(union (populate_ins_outs ir ins_outs) ir_cong) in
  let ir = Types.Preassign.run ir ~f:preassign in
  Log.send "VIBES IR:\n%a" Ir.pp ir;
  ir
