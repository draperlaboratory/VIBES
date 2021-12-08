(* Implements {!Compiler}. *)

open !Core_kernel
open Bap_knowledge
open Bap_core_theory
open Bap.Std
open Knowledge.Syntax
open Knowledge.Let
open Graphlib.Std

module KB = Knowledge
module Arm = Arm_selector
module Subst = Substituter
module Hvar = Higher_var

(* Applies the peephole optimizer to the output of a given solver. *)
let optimized solver =
  function ir ->
    let+ (ir, sol) = solver ir in
    (Arm.peephole ir, sol)

(* Converts a list of BIR statements to a list of ARM assembly strings. *)
let create_assembly (solver : Ir.t -> (Ir.t * Minizinc.sol) KB.t)
    (ir : Ir.t) : (string list * Minizinc.sol) KB.t =
  optimized solver ir >>= fun (ir, new_sol) ->
  let pretty_ir = Arm.Pretty.arm_ir_pretty ir in
  match pretty_ir with
  | Ok assembly -> KB.return (assembly, new_sol)
  | Error e -> Kb_error.fail e

let split_word (w : word) : word * word * word =
  let shift = Word.of_int ~width:32 16 in
  let lower = Word.(w land of_int ~width:32 0xFFFF) in
  let upper = Word.(w lsr shift) in
  shift, lower, upper

(* On ARM, we can't use constants larger than 65535. The typical idiom is
   to load the lower 16 bits first, then load the upper 16 bits, using a
   movw/movt idiom.

   TODO: clean this up and make it more general, right now I'm only
   covering a limited number of cases.
*)
let split_large_const (blks : Blk.t list) : Blk.t list =
  let new_tmp () =
    Var.create ~is_virtual:true ~fresh:true "const" (Type.Imm 32) in
  let idiom v shift upper =
    Bil.(var v lor (int upper lsl int shift)) in
  List.map blks ~f:(fun blk ->
      let tbl = Tid.Table.create () in
      let saved_temps = Word.Table.create () in
      let new_defs = ref [] in
      (* Handle defs *)
      let blk = Term.map def_t blk ~f:(fun def ->
          match Def.rhs def with
          | Int w when Word.to_int_exn w > 0xFFFF -> begin
              match Word.Table.find saved_temps w with
              | Some v -> Def.with_rhs def @@ Var v
              | None ->
                let shift, lower, upper = split_word w in
                let lhs = Def.lhs def in
                begin
                  let def1 = Def.create lhs Bil.(int lower) in
                  Tid.Table.change tbl (Term.tid def) ~f:(function
                      | None -> Some [def1]
                      | Some _ -> assert false)
                end;
                Def.with_rhs def @@ idiom lhs shift upper
            end
          | Load (mem, Int w, endian, size) when Word.to_int_exn w > 0xFFFF ->
            let shift, lower, upper = split_word w in
            let lhs = Def.lhs def in
            begin
              let def1 = Def.create lhs Bil.(int lower) in
              let def2 = Def.create lhs @@ idiom lhs shift upper in
              Tid.Table.change tbl (Term.tid def) ~f:(function
                  | None -> Some [def2; def1]
                  | Some _ -> assert false)
            end;
            Def.with_rhs def @@ Load (mem, Var lhs, endian, size)
          | Store (mem, Int w, value, endian, size)
            when Word.to_int_exn w > 0xFFFF ->
            let shift, lower, upper = split_word w in
            let tmp = new_tmp () in
            begin
              let def1 = Def.create tmp Bil.(int lower) in
              let def2 = Def.create tmp @@ idiom tmp shift upper in
              Tid.Table.change tbl (Term.tid def) ~f:(function
                  | None -> Some [def2; def1]
                  | Some _ -> assert false)
            end;
            (* Word.Table.set saved_temps ~key:w ~data:tmp; *)
            Def.with_rhs def @@ Store (mem, Var tmp, value, endian, size)
          | _ -> def)
      in
      (* Handle jmps (right now only the cond of the jmp) *)
      let blk = Term.map jmp_t blk ~f:(fun jmp ->
          match Jmp.cond jmp with
          | BinOp (op, Load (mem, Int w, endian, size), rhs)
            when Word.to_int_exn w > 0xFFFF ->
            let shift, lower, upper = split_word w in
            let tmp = new_tmp () in
            begin
              let def1 = Def.create tmp Bil.(int lower) in
              let def2 = Def.create tmp @@ idiom tmp shift upper in
              new_defs := !new_defs @ [def1; def2]
            end;
            Jmp.with_cond jmp @@
            BinOp (op, Load (mem, Var tmp, endian, size), rhs)
          | _ -> jmp) in
      (* Prepend new defs (from previous defs) *)
      let blk =
        Tid.Table.fold tbl ~init:blk ~f:(fun ~key:tid ~data:defs blk ->
            List.fold defs ~init:(blk, tid) ~f:(fun (blk, before) def ->
                Term.prepend def_t blk def ~before, Term.tid def) |> fst)
      in
      (* Append new defs (from jmps) *)
      List.fold !new_defs ~init:blk ~f:(fun blk def ->
          Term.append def_t blk def))

(* On Thumb, the selector will try to create a conditional `bl` instruction,
   which is illegal, so we need to insert a new block for the call, with
   a conditional goto to this new block. *)
let massage_conditional_calls (blks : Blk.t list) : Blk.t list =
  let new_blks = ref [] in
  let blks =
    List.map blks ~f:(Term.map jmp_t ~f:(fun jmp ->
        match Jmp.kind jmp with
        | Call call when Exp.(Jmp.cond jmp <> Int Word.b1) ->
          let builder = Blk.Builder.create () in
          Blk.Builder.add_jmp builder (Jmp.create_call call);
          let result = Blk.Builder.result builder in
          new_blks := result :: !new_blks;
          Jmp.with_kind jmp @@ Goto (Direct (Term.tid result))
        | _ -> jmp)) in
  blks @ !new_blks

(* Order the blocks according to a reverse postorder DFS traversal.
   This should minimize the number of extra jumps we need to insert. *)
let reorder_blks (blks : Blk.t list) : Blk.t list =
  let sub = List.fold blks ~init:(Sub.create () ~name:"dummy-wrapper")
      ~f:(Term.append blk_t) in
  let cfg = Sub.to_cfg sub in
  Graphlib.reverse_postorder_traverse (module Graphs.Ir) cfg |>
  Seq.to_list |> List.map ~f:Graphs.Ir.Node.label

let to_ssa (blks : Blk.t list) : Blk.t list =
  (* Create the subroutine, which will fill in the control-flow edges. *)
  let sub = List.fold blks ~init:(Sub.create () ~name:"dummy-wrapper")
      ~f:(Term.append blk_t) in
  (* Convert to SSA. *)
  let sub = Sub.ssa sub in
  (* TODO - try this: *)
  let sub = Linear_ssa.transform sub in
  Term.enum blk_t sub |> Seq.to_list

(* Spill higher vars in caller-save registers if we are doing any calls
   in a multi-block patch, since the ABI says they may be clobbered.

   XXX: what if other Hvars rely on offsets from SP? We should probably just fail.
   Write a function that checks if such hvars are already present.
*)
let spill_hvars (tgt : Theory.target) (sp_align : int)
    (hvars : Hvar.t list) (entry_blk : Blk.t) (exit_blk : Blk.t)
    (blks : Blk.t list) : Blk.t list * Higher_var.t list =
  let has_calls = List.exists blks ~f:(fun blk ->
      Term.enum jmp_t blk |> Seq.exists ~f:(fun jmp ->
          match Jmp.kind jmp with
          | Call _ -> true
          | _ -> false)) in
  if not (has_calls && List.length blks > 1) then blks, hvars
  else
    (* We're going to use predetermined stack locations, for simplicity.
       The nice part is that the total space will be a multiple of 8. *)
    let caller_save = String.Map.of_alist_exn [
        "R0", (Word.zero 32, Arm_env.r0);
        "R1", (Word.of_int ~width:32 4, Arm_env.r1);
        "R2", (Word.of_int ~width:32 8, Arm_env.r2);
        "R3", (Word.of_int ~width:32 12, Arm_env.r3);
      ] in
    let spilled = ref String.Set.empty in
    (* Find which hvars we need to spill. *)
    let hvars = List.map hvars ~f:(fun hvar ->
        let name = Hvar.name hvar in
        let value = Hvar.value hvar in
        match Hvar.at_entry value with
        | None -> hvar
        | Some at_entry ->
          match Hvar.at_exit value with
          | None -> hvar
          | Some at_exit ->
            match Hvar.register at_entry, Hvar.register at_exit with
            | Some v, Some v' when String.(v = v') ->
              begin
                match Map.find caller_save v with
                | None -> hvar
                | Some (offset, _) ->
                  let memory = Hvar.create_frame "SP" offset in
                  let at_entry = Hvar.stored_in_memory memory in
                  spilled := Set.add !spilled v;
                  Hvar.create_with_storage name ~at_entry ~at_exit:None
              end
            | _ -> hvar) in
    (* Don't bother if nothing got spilled. *)
    if Set.is_empty !spilled then blks, hvars
    else
      let mem = Var.reify @@ Theory.Target.data tgt in
      let endian =
        if Theory.(Endianness.(Target.endianness tgt = eb))
        then BigEndian else LittleEndian in
      let sp = Arm_env.sp in
      (* Predetermined amount of space to allocate on the stack. *)
      let space = Word.of_int ~width:32 (sp_align + 16) in
      let push v =
        let open Bil.Types in
        let off, reg = Map.find_exn caller_save v in
        let addr = BinOp (PLUS, Var sp, Int off) in
        Def.create mem @@ Store (Var mem, addr, Var reg, endian, `r32);
      in
      let pop v =
        let open Bil.Types in
        let off, reg = Map.find_exn caller_save v in
        let addr = BinOp (PLUS, Var sp, Int off) in
        Def.create reg @@ Load (Var mem, addr, endian, `r32);
      in
      let pushes =
        Set.to_list !spilled |> List.rev |> List.map ~f:push in
      let _pops =
        Set.to_list !spilled |> List.rev |> List.map ~f:pop in
      let blks = List.map blks ~f:(fun blk ->
          let tid = Term.tid blk in
          if Tid.(tid = Term.tid entry_blk) then
            let blk = List.fold pushes ~init:blk ~f:(fun blk def ->
                Term.prepend def_t blk def) in
            let adj = Def.create sp @@ BinOp (MINUS, Var sp, Int space) in
            Term.prepend def_t blk adj
          else if Tid.(tid = Term.tid exit_blk) then
            (* let blk = List.fold pops ~init:blk ~f:(fun blk def -> *)
            (*     Term.append def_t blk def) in *)
            let adj = Def.create sp @@ BinOp (PLUS, Var sp, Int space) in
            Term.append def_t blk adj

          else blk)
      in
      blks, hvars

(* If we've specified that we want a higher var to remain in a callee-save register
   for the lifetime of the patch, then we need to tell minizinc to never use it
   in a solution. *)
let collect_exclude_regs (hvars : Hvar.t list) : String.Set.t =
  let is_callee_save = function
    | "R4" | "R5" | "R6" | "R7" | "R8" -> true
    | _ -> false in
  List.fold hvars ~init:String.Set.empty ~f:(fun acc hvar ->
      let value = Hvar.value hvar in
      match Hvar.at_entry value with
      | None -> acc
      | Some at_entry ->
        match Hvar.at_exit value with
        | None -> acc
        | Some at_exit ->
          match Hvar.register at_entry, Hvar.register at_exit with
          | Some v, Some v' when String.(v = v') && is_callee_save v ->
            String.Set.add acc v
          | _ -> acc)

(* Converts a list of BIR statements to a list of ARM assembly strings. *)
let create_vibes_ir
    (tgt: Theory.target)
    (lang : Theory.language)
    (hvars : Higher_var.t list)
    (sp_align : int)
    (bir : Insn.t) : (Ir.t * String.Set.t) KB.t =
  let ir = Blk.from_insns [bir] in
  (* BAP will give us the blks in such an order that the first one is the
     entry blk. *)
  let entry_blk = List.hd_exn ir in
  let exit_blk = List.last_exn ir in
  let ir, hvars = spill_hvars tgt sp_align hvars entry_blk exit_blk ir in
  let exclude_regs = collect_exclude_regs hvars in
  let ir = Bir_opt.apply ir in
  let* ir = Subst.substitute tgt hvars ir in
  let ir = split_large_const ir in
  let ir =
    if Arm_selector.is_thumb lang
    then massage_conditional_calls ir
    else ir in
  let ir = reorder_blks ir in
  let ir = Bir_opt.apply_ordered ir in
  let ir = to_ssa ir in
  Events.(send @@ Info "SSA'd BIR\n");
  Events.(send @@ Info (
      List.map ir ~f:(fun blk -> Format.asprintf "    %a" Blk.pp blk) |>
      String.concat ~sep:"\n"));
  Events.(send @@ Info "\n\n");
  let* ir = Arm.ARM_Gen.select lang ir in
  let ir = Arm.preassign tgt lang ir in
  KB.return (ir, exclude_regs)

(* Compile one patch from BIR to VIBES IR *)
let compile_one_vibes_ir (count : int KB.t) (patch : Data.Patch.t) : int KB.t =
  count >>= fun n -> Data.Patch.get_assembly patch >>= begin function
    | Some _asm ->
      Events.(send @@ Info "The patch has no IR to translate.\n");
      KB.return () (* Assembly already set. Presumably by the user. *)
    | None ->
      let info_str =
        Format.sprintf "Translating patch %d BIR to VIBES IR..." n
      in
      Events.(send @@ Info info_str);
      Data.Patch.get_bir patch >>= fun bir ->

      let info_str = Format.asprintf "\nPatch: %a\n\n%!" KB.Value.pp bir in
      Events.(send @@ Info info_str);

      Data.Patch.get_lang patch >>= fun lang ->
      Data.Patch.get_target patch >>= fun tgt ->
      Data.Patch.get_patch_vars_exn patch >>= fun hvars ->
      Data.Patch.get_sp_align_exn patch >>= fun sp_align ->
      create_vibes_ir tgt lang hvars sp_align bir >>= fun (ir, exclude_regs) ->
      Data.Patch.set_raw_ir patch (Some ir) >>= fun () ->
      Data.Patch.set_exclude_regs patch (Some exclude_regs) >>= fun () ->
      Events.(send @@ Info "The patch has the following VIBES IR:\n");
      Events.(send @@ Rule);
      Events.(send @@ Info (Ir.pretty_ir ir));
      Events.(send @@ Rule);
      KB.return ()
  end >>= fun () -> KB.return (n + 1)

(* Compile one patch from VIBES IR to assembly *)
let compile_one_assembly
    (solver :
       ?exclude_regs:String.Set.t ->
     Theory.target ->
     Theory.language ->
     Minizinc.sol list ->
     Ir.t ->
     (Ir.t * Minizinc.sol) KB.t)
    (count : int KB.t) (patch : Data.Patch.t) : int KB.t =
  count >>= fun n -> Data.Patch.get_assembly patch >>= begin function
    | Some _asm ->
      Events.(send @@ Info "The patch already has assembly\n");
      Events.(send @@ Rule);
      KB.return () (* Assembly already set. Presumably by the user. *)
    | None ->
      let info_str =
        Format.asprintf "Translating patch %s VIBES IR to assembly..."
          (string_of_int n)
      in
      Events.(send @@ Info info_str);
      Data.Patch.get_raw_ir_exn patch >>= fun ir ->
      Data.Patch.get_exclude_regs patch >>= fun exclude_regs ->
      let exclude_regs = Option.value exclude_regs ~default:String.Set.empty in
      Data.Patch.get_minizinc_solutions patch >>= fun prev_sols ->
      Data.Patch.get_target patch >>= fun target ->
      Data.Patch.get_lang patch >>= fun lang ->
      let prev_sols = Set.to_list prev_sols in
      create_assembly
        (solver target lang prev_sols ~exclude_regs)
        ir >>= fun (assembly, new_sol) ->
      Data.Patch.set_assembly patch (Some assembly) >>= fun () ->
      Events.(send @@ Info "The patch has the following assembly:\n");
      Events.(send @@ Rule);
      Events.(send @@ Info (String.concat ~sep:"\n" assembly));
      Events.(send @@ Rule);
      Data.Patch.add_minizinc_solution patch new_sol 
  end >>= fun () -> KB.return (n + 1)

(* Converts the patch (as BIR) to VIBES IR instructions. *)
let compile_ir (obj : Data.t) : unit KB.t =
  Events.(send @@ Header "Starting IR compiler");
  Data.Patched_exe.get_patches obj >>= fun patches ->
  let size : string = string_of_int (Data.Patch_set.length patches) in
  Events.(send @@ Info ("There are " ^ size ^ " patch fragments."));
  Data.Patch_set.fold patches ~init:(KB.return 1)
    ~f:(compile_one_vibes_ir) >>= fun _ ->
  Events.(send @@ Info "Done.");
  KB.return ()

(* Converts the patch (as IR) to assembly instructions. *)
let compile_assembly ?solver:(solver = Minizinc.run_minizinc) (obj : Data.t)
  : unit KB.t =
  Events.(send @@ Header "Starting Minizinc compiler");
  Data.Solver.get_minizinc_model_filepath_exn obj >>= fun mzn_model ->
  Events.(send @@ Info ("Using minizinc model: " ^ mzn_model));
  Data.Patched_exe.get_patches obj >>= fun patches ->
  let size : string = string_of_int (Data.Patch_set.length patches) in
  Events.(send @@ Info ("There are " ^ size ^ " patch fragments."));
  Data.Patch_set.fold patches ~init:(KB.return 1)
    ~f:(compile_one_assembly (solver ~filepath:mzn_model)) >>= fun _ ->
  Events.(send @@ Info "Done.");
  KB.return ()
