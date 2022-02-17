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

(* Implements {!Bir_opt}. *)

open !Core_kernel
open Bap.Std
open Bap_core_theory
open Graphlib.Std
open KB.Let

module Arm = Arm_selector
module Subst = Substituter
module Hvar = Higher_var
module Err = Kb_error

type t = {
  ir : blk term list;
  exclude_regs : String.Set.t;
  argument_tids : Tid.Set.t;
}

(* Helper functions *)
module Helper = struct

  (* Create a dummy subroutine from the blocks. *)
  let create_sub (blks : blk term list) : sub term KB.t =
    let+ tid = Theory.Label.fresh in
    Sub.create ~name:"dummy-wrapper" ~blks ~tid ()

  (* Find the exit nodes of the patch code. *)
  let exit_blks (blks : blk term list) : blk term list KB.t =
    match blks with
    | [_] -> KB.return blks
    | _ ->
      let+ sub = create_sub blks in
      let cfg = Sub.to_cfg sub in
      Graphs.Ir.nodes cfg |> Seq.to_list |>
      List.filter_map ~f:(fun node ->
          let blk = Graphs.Ir.Node.label node in
          if Graphs.Ir.Node.degree node cfg ~dir:`Out = 0
          then Some blk
          else
            let jmps = Term.enum jmp_t blk in
            if Seq.exists jmps ~f:(fun jmp ->
                match Jmp.kind jmp with
                | Call call -> begin
                    match Call.return call with
                    | None | Some (Indirect _) -> true
                    | _ -> false
                  end
                | _ -> false)
            then Some blk
            else None)

  (* Returns true if the block contains a call to a subroutine. *)
  let has_call (blk : blk term) : bool =
    Term.enum jmp_t blk |> Seq.exists ~f:(fun jmp ->
        match Jmp.kind jmp with
        | Call _ -> true
        | _ -> false)

  (* Returns true if there are calls in the IR. *)
  let has_calls (blks : blk term list) : bool =
    List.exists blks ~f:has_call

  (* Returns true if the jmp is unconditional. *)
  let is_unconditional (jmp : jmp term) : bool =
    match Jmp.cond jmp with
    | Int w -> Word.(w = b1)
    | _ -> false
  
end

(* Optimizations *)
module Opt = struct

  type t = blk term list -> blk term list

  (* Returns the block that is the destination of the jump, if it is in
     the supplied list, and the jump is direct. returns [None]
     otherwise. *)
  let find_tgt (blks : blk term list) (jmp : jmp term) : blk term option =
    let dst = Jmp.dst jmp in
    match Option.map ~f:Jmp.resolve dst with
    | Some (First tid) ->
      List.find blks ~f:(fun blk -> Tid.(Term.tid blk = tid))
    | _ -> None

  (* If the input block is a single goto statement, returns the
     destination of that jump. Returns [None] otherwise. *)
  let is_redirect (blk : blk term) : Jmp.dst option =
    match Blk.elts blk |> Seq.to_list with
    | [`Jmp jmp] ->
      let cond = Jmp.cond jmp in
      let is_unconditional =
        match cond with
        | Bil.Types.Int w -> Word.is_one w
        | _ -> false
      in
      if is_unconditional then
        Option.first_some
          (Jmp.dst jmp)
          (Jmp.alt jmp)
      else None
    | _ -> None

  (* If a jump can be short circuited (it's destination is a single goto
     statement), then return the jump which goes to the next
     destination. Otherwise return [None].

     Note that the optimization does not compute "final" targets, as
     this seems to rarely be useful and would need additional logic to
     handle loops. *)
  let short_circ_jmp
      (blks : blk term list) (jmp : jmp term) : jmp term option =
    match find_tgt blks jmp with
    | None -> None
    | Some blk ->
      begin
        match is_redirect blk with
        | None -> None
        | Some dst ->
          Some (Jmp.with_dst jmp (Some dst))
      end

  let short_circ_blk (blks : blk term list) (blk : blk term) : blk term =
    Term.map jmp_t blk
      ~f:(fun jmp ->
          match short_circ_jmp blks jmp with
          | None -> jmp
          | Some jmp' -> jmp')

  let short_circ : t = fun blks -> List.map blks ~f:(short_circ_blk blks)

  let simplify_exp : exp -> exp = Exp.simpl ~ignore:Eff.[read]

  let simplify_blk (blk : blk term) : blk term =
    Term.map def_t blk ~f:(fun def ->
        Def.with_rhs def @@ simplify_exp @@ Def.rhs def) |>
    Term.map jmp_t ~f:(fun jmp ->
        Jmp.with_cond jmp @@ simplify_exp @@ Jmp.cond jmp)

  let simplify : t =  List.map ~f:simplify_blk

  (* Applies all the optimizations in the list *)
  let apply_list (opts : t list) : t = fun init ->
    List.fold opts ~init ~f:(fun ir opt -> opt ir)

  (* Applies all the optimizations we currently perform. *)
  let apply : t = apply_list [
      simplify;
      short_circ;
    ]

  (* Attempt to merge adjacent blocks which have an edge in between them.
     For this transformation, the blks must be ordered according to a
     reverse postorder DFS traversal.

     NOTE: after this, generating a CFG (using `Sub.to_cfg`) has some
     caveats. Fallthrough edges will be made implicit by the ordering of
     the blocks, and thus they will not be present in the generated CFG.
 *)
  let merge_adjacent (ir : blk term list) : blk term list =
    (* `finished_blks` are blocks we will not try to optimize further.
       They are to be inserted in reverse order of `ir`. *)
    let rec aux ~finished_blks = function
      | [] -> List.rev finished_blks
      | [blk] -> List.rev (blk :: finished_blks)
      | blk1 :: blk2 :: rest ->
        let jmps1 = Term.enum jmp_t blk1 |> Seq.to_list in
        (* We're starting with the last jmp first. We will attempt to
           pop it from the blk, and see if further optimization can be
           performed. *)
        match List.last jmps1 with
        | Some jmp when Helper.is_unconditional jmp -> begin
            match Jmp.kind jmp with
            | Goto (Direct tid) when Tid.(tid = Term.tid blk2) -> begin
                match jmps1 with
                | [_] ->
                  (* It's the only jmp in the blk, so merge the two
                     adjacent blocks together. Then, try to optimize
                     the merged block. *)
                  let phis1 = Term.enum phi_t blk1 |> Seq.to_list in
                  let phis2 = Term.enum phi_t blk2 |> Seq.to_list in
                  let defs1 = Term.enum def_t blk1 |> Seq.to_list in
                  let defs2 = Term.enum def_t blk2 |> Seq.to_list in
                  let jmps2 = Term.enum jmp_t blk2 |> Seq.to_list in
                  let new_blk =
                    Blk.create
                      ~phis:(phis1 @ phis2)
                      ~defs:(defs1 @ defs2)
                      ~jmps:jmps2
                      ~tid:(Term.tid blk1) () in
                  aux ~finished_blks (new_blk :: rest)
                | _ ->
                  (* It wasn't the only jmp in the blk, so just remove it,
                     and try optimizing it again. *)
                  let blk1 = Term.remove jmp_t blk1 @@ Term.tid jmp in
                  aux ~finished_blks (blk1 :: blk2 :: rest)
              end
            | _ -> aux ~finished_blks:(blk1 :: finished_blks) (blk2 :: rest)
          end
        | _ -> aux ~finished_blks:(blk1 :: finished_blks) (blk2 :: rest) in
    aux ~finished_blks:[] ir

end

(* Change the shape of the code for the instruction selector. *)
module Shape = struct

  (* If there are exit nodes of the form `call ... with noreturn` or
     `call ... with return <indirect>`, then insert a new blk for them
     to return to. *)
  let adjust_noreturn_exits (blks : blk term list) : blk term list KB.t =
    let* exits = Helper.exit_blks blks in
    let exits = List.map exits ~f:Term.tid |> Tid.Set.of_list in
    (* This is the block that we may insert as a continuation
       for `call ... with noreturn`. Since such calls don't have any
       specific destination for the return, we can just re-use this
       block for that purpose. *)
    let extra_noret = ref None in
    (* Returns to indirect targets, on the other hand, require us
       to make a unique block for each target. *)
    let extra_indirect = ref [] in
    (* Maps jmp tids to blk tids. The blk tids refer to the new
       return destination for the jmp. *)
    let rewrite_tbl = Tid.Table.create () in
    (* Collect information about jmps that need to be rewrtitten. *)
    let+ () = KB.List.iter blks ~f:(fun blk ->
        let tid = Term.tid blk in
        if Set.mem exits tid then
          Term.enum jmp_t blk |> KB.Seq.iter ~f:(fun jmp ->
              let tid = Term.tid jmp in
              match Jmp.kind jmp with
              | Call call -> begin
                  match Call.return call with
                  | Some (Direct _) -> KB.return ()
                  | Some (Indirect _ as label) ->
                    let+ tid' = Theory.Label.fresh in
                    let blk' = Blk.create ~tid:tid' ~jmps:[
                        Jmp.create_goto label;
                      ] () in
                    extra_indirect := blk' :: !extra_indirect;
                    Tid.Table.set rewrite_tbl ~key:tid ~data:tid'
                  | None ->
                    let+ tid' = match !extra_noret with
                      | Some blk' -> KB.return @@ Term.tid blk'
                      | None ->
                        let+ tid' = Theory.Label.fresh in
                        let blk' = Blk.create ~tid:tid' () in
                        extra_noret := Some blk';
                        tid' in
                    Tid.Table.set rewrite_tbl ~key:tid ~data:tid'
                end
              | _ -> KB.return ())
        else KB.return ()) in
    (* Rewrite the return destination for each call. *)
    let blks = List.map blks ~f:(fun blk ->
        Term.map jmp_t blk ~f:(fun jmp ->
            let tid = Term.tid jmp in
            match Tid.Table.find rewrite_tbl tid with
            | Some tid' -> Jmp.(with_dst jmp @@ Some (resolved tid'))
            | None -> jmp)) in
    (* Append the generated blocks. *)
    let extra = Option.value_map !extra_noret ~default:[] ~f:List.return in
    blks @ extra @ !extra_indirect

  (* Order the blocks according to a reverse postorder DFS traversal.
     This should minimize the number of extra jumps we need to insert. *)
  let reorder_blks (blks : blk term list) : blk term list KB.t =
    let+ sub = Helper.create_sub blks in
    let cfg = Sub.to_cfg sub in
    Graphlib.reverse_postorder_traverse (module Graphs.Ir) cfg |>
    Seq.to_list |> List.map ~f:Graphs.Ir.Node.label

end

(* Handle storage classification in the presence of ABI information. *)
module ABI = struct

  (* Collect the args to the call, if any. *)
  let collect_args (blk : blk term) : var list option KB.t =
    Term.enum jmp_t blk |> KB.Seq.find_map ~f:(fun jmp ->
        match Jmp.alt jmp with
        | None -> KB.return None
        | Some dst -> match Jmp.resolve dst with
          | Second _ -> KB.return None
          | First tid ->
            let+ args = Core_c.collect_args tid in
            Some args)

  (* Collect the tids where the arguments to calls are defined. *)
  let collect_argument_tids (blks : blk term list) : Tid.Set.t KB.t =
    KB.List.fold blks ~init:Tid.Set.empty ~f:(fun acc blk ->
        let+ args = collect_args blk in
        Option.value_map args ~default:acc ~f:(fun args ->
            Term.enum def_t blk |> Seq.to_list |>
            List.fold_right ~init:(Var.Set.of_list args, acc)
              ~f:(fun def (remaining, acc) ->
                  let lhs = Def.lhs def in
                  if Var.Set.mem remaining lhs then (
                    Var.Set.remove remaining lhs,
                    Tid.Set.add acc @@ Term.tid def
                  ) else (remaining, acc)) |> snd))

  (* Create a fake memory assignment which is a signpost to the selector
     that the most recently assigned memory is a dependency (or "argument")
     of the function call. *)
  let insert_new_mems_at_callsites (tgt : Theory.target)
      (blks : blk term list) : (blk term list * Tid.Set.t) KB.t =
    let mem = Var.reify @@ Theory.Target.data tgt in
    let+ blks, tids = KB.List.fold blks ~init:([], Tid.Set.empty)
        ~f:(fun (blks, tids) blk ->
            if Helper.has_call blk then
              let+ tid = Theory.Label.fresh in
              let lhs = Var.create (Var.name mem ^ "_call") (Var.typ mem) in
              let def = Def.create ~tid lhs @@ Var mem in
              let blk = Term.append def_t blk def in
              blk :: blks, Tid.Set.add tids tid
            else KB.return (blk :: blks, tids)) in
    List.rev blks, tids

  (* We shouldn't do any spilling if there are higher vars that depend
     on SP-relative locations in memory. *)
  let check_hvars_for_existing_stack_locations
      (sp : var) (hvars : Hvar.t list) : unit KB.t =
    KB.List.iter hvars ~f:(fun hvar ->
        match Hvar.value hvar with
        | Hvar.(Storage {at_entry = Memory (Frame (reg, offset)); _}) ->
          if String.(reg <> Var.name sp) then KB.return ()
          else Err.(fail @@ Other (
              sprintf "Existing stack location [%s, %s] used by hvar %s"
                reg (Word.to_string offset) (Hvar.name hvar)))
        | _ -> KB.return ())

  (* Spill higher vars in caller-save registers if we are doing any calls
     in a multi-block patch, since the ABI says they may be clobbered. *)
  let spill_hvars_and_adjust_stack (tgt : Theory.target) (sp_align : int)
      (hvars : Hvar.t list) (entry_blk : blk term)
      (blks : blk term list) : (blk term list * Higher_var.t list) KB.t =
    if not (Helper.has_calls blks && List.length blks > 1)
    then KB.return (blks, hvars)
    else
      let width = Theory.Target.bits tgt in
      let stride = Word.of_int ~width (width lsr 3) in
      (* Use predetermined stack locations. *)
      let caller_save =
        Theory.Target.regs tgt ~roles:Theory.Role.Register.[caller_saved] |>
        Set.to_list |> List.mapi ~f:(fun i v ->
            let idx = Word.of_int ~width i in
            let v = Var.reify v in
            let name = Var.name v in
            name, (Word.(stride * idx), v)) |>
        String.Map.of_alist_exn in
      let* sp =
        match Theory.Target.reg tgt Theory.Role.Register.stack_pointer with
        | Some v -> KB.return @@ Var.reify v
        | None -> Err.(fail @@ Other (
            sprintf "No stack pointer register for target %s\n%!"
              (Theory.Target.to_string tgt))) in
      let spilled = ref String.Set.empty
      and restored = ref String.Set.empty in
      (* Find which hvars we need to spill. *)
      let spill ?(restore = false) name v offset =
        let memory = Hvar.create_frame (Var.name sp) offset in
        let at_entry = Hvar.stored_in_memory memory in
        spilled := Set.add !spilled v;
        if restore then restored := Set.add !restored v;
        Hvar.create_with_storage name ~at_entry ~at_exit:None in
      let* hvars' = KB.List.map hvars ~f:(fun hvar ->
          let name = Hvar.name hvar in
          match Hvar.value hvar with
          | Hvar.Storage {at_entry = Register v; at_exit} -> begin
              match Map.find caller_save v with
              | None -> KB.return hvar
              | Some (offset, _) ->
                match at_exit with
                | Some Hvar.(Register v') when String.(v = v') ->
                  KB.return @@ spill name v offset ~restore:true
                | Some _ ->
                  Err.(fail @@ Other (
                      sprintf "Unexpected value for `at_exit` of higher var %s"
                        name))
                | None -> KB.return @@ spill name v offset
            end
          | _ -> KB.return hvar) in
      let* exits = Helper.exit_blks blks in
      let exits = List.map exits ~f:Term.tid |> Tid.Set.of_list in
      let no_regs = Set.is_empty !spilled && Set.is_empty !restored in
      let* () = if no_regs then KB.return ()
        else check_hvars_for_existing_stack_locations sp hvars in
      if no_regs && sp_align = 0
      then KB.return (blks, hvars')
      else
        let mem = Var.reify @@ Theory.Target.data tgt in
        let endian =
          if Theory.(Endianness.(Target.endianness tgt = eb))
          then BigEndian else LittleEndian in
        (* Predetermined amount of space to allocate on the stack. *)
        let space =
          Map.length caller_save * (width lsr 3) |>
          Int.round_up ~to_multiple_of:(Theory.Target.data_alignment tgt) in
        let space = Word.of_int ~width @@
          if no_regs then sp_align else sp_align + space in
        let sp = Substituter.mark_reg sp in
        let push v =
          let open Bil.Types in
          let off, reg = Map.find_exn caller_save v in
          let reg = Substituter.mark_reg reg in
          let addr = BinOp (PLUS, Var sp, Int off) in
          let+ tid = Theory.Label.fresh in
          Def.create ~tid mem @@ Store (Var mem, addr, Var reg, endian, `r32) in
        let pop v =
          let open Bil.Types in
          let off, reg = Map.find_exn caller_save v in
          let reg = Substituter.mark_reg reg in
          let addr = BinOp (PLUS, Var sp, Int off) in
          let+ tid = Theory.Label.fresh in
          Def.create ~tid reg @@ Load (Var mem, addr, endian, `r32) in
        let* pushes = Set.to_list !spilled |> List.rev |> KB.List.map ~f:push in
        let* pops = Set.to_list !restored |> List.rev |> KB.List.map ~f:pop in
        let+ blks = KB.List.map blks ~f:(fun blk ->
            let tid = Term.tid blk in
            if Tid.(tid = Term.tid entry_blk) then
              let blk = List.fold pushes ~init:blk ~f:(fun blk def ->
                  Term.prepend def_t blk def) in
              let+ tid = Theory.Label.fresh in
              let adj = Def.create ~tid sp @@ BinOp (MINUS, Var sp, Int space) in
              Term.prepend def_t blk adj
            else if Set.mem exits tid then
              let blk = List.fold pops ~init:blk ~f:(fun blk def ->
                  Term.append def_t blk def) in
              let+ tid = Theory.Label.fresh in
              let adj = Def.create ~tid sp @@ BinOp (PLUS, Var sp, Int space) in
              Term.append def_t blk adj
            else KB.return blk) in
        blks, hvars'

  (* If we've specified that we want a higher var to remain in a callee-save
     register for the lifetime of the patch, then we need to tell minizinc
     to never use it in a solution. *)
  let collect_exclude_regs (tgt : Theory.target)
      (hvars : Hvar.t list) : String.Set.t =
    let callee_save =
      Theory.Target.regs tgt ~roles:Theory.Role.Register.[callee_saved] |>
      Set.to_list |>
      List.map ~f:(fun v -> Var.name @@ Var.reify v) |>
      String.Set.of_list in
    List.fold hvars ~init:String.Set.empty ~f:(fun acc hvar ->
        match Hvar.value hvar with
        | Hvar.(Storage {at_entry = Register v; at_exit = Some (Register v')})
          when String.(v = v') && Set.mem callee_save v ->
          String.Set.add acc v
        | _ -> acc)

end

(* VIBES IR requires linear SSA form. *)
let to_linear_ssa
    (patch : Data.Patch.t)
    (blks : blk term list) : blk term list KB.t =
  let* sub = Helper.create_sub blks in
  sub |> Sub.ssa |> Linear_ssa.transform ~patch:(Some patch)

let run (patch : Data.Patch.t) : t KB.t =
  let* code = Data.Patch.get_bir patch in
  let info_str = Format.asprintf "\nPatch: %a\n\n%!" KB.Value.pp code in
  Events.(send @@ Info info_str);
  let* tgt = Data.Patch.get_target patch in
  let* sp_align = Data.Patch.get_sp_align_exn patch in
  let* hvars = Data.Patch.get_patch_vars_exn patch in
  let ir = Blk.from_insns [code] in
  (* BAP will give us the blks in such an order that the first one is the
     entry blk. *)
  let* entry_blk = match ir with
    | blk :: _ -> KB.return blk
    | [] -> Err.(fail @@ Other (
        "Bir_passes: Blk.from_insns returned an empty list of blks")) in
  let* argument_tids = ABI.collect_argument_tids ir in
  let* ir, mem_argument_tids = ABI.insert_new_mems_at_callsites tgt ir in
  let argument_tids = Tid.Set.union argument_tids mem_argument_tids in
  let* ir = Shape.adjust_noreturn_exits ir in
  let* ir, hvars =
    ABI.spill_hvars_and_adjust_stack tgt sp_align hvars entry_blk ir in
  let exclude_regs = ABI.collect_exclude_regs tgt hvars in
  let ir = Opt.apply ir in
  let* ir = Subst.substitute tgt hvars ir in
  let* ir = Shape.reorder_blks ir in
  let ir = Opt.merge_adjacent ir in
  let+ ir = to_linear_ssa patch ir in
  {ir; exclude_regs; argument_tids}
