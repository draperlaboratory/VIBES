open Core
open Bap.Std

module T = Bap_core_theory.Theory
module Err = Vibes_error_lib.Std
module Function_info = Vibes_function_info_lib.Types
module Hvar = Vibes_higher_vars_lib.Higher_var
module Naming = Vibes_higher_vars_lib.Substituter.Naming
module Bir_helpers = Vibes_bir_lib.Helpers

open Vibes_error_lib.Let

(* Collect the args to the call, if any. *)
let collect_args ~(target : T.Target.t) 
    ~(func_info : Function_info.t)
    (blk : blk term) : var list option =
  let jmps = Term.enum jmp_t blk in
  let match_jmp jmp =
    (* Is the jmp dst outside the current function? That's a call. *)
    match Jmp.alt jmp with
    | None -> None
    | Some dst ->
       begin
         (* The resolved dst is either a tid or a bitv. *)
         match Jmp.resolve dst with
         | First tid ->
           begin
             match Function_info.find func_info ~tid with
             | Some arg_names ->
               Some (Function_info.vars_of_args arg_names ~target)
             | None -> None
           end
         | Second _ -> None (* We don't handle indirect calls *)
       end
  in
  Seq.find_map jmps ~f:match_jmp

(* Collect the tids where the arguments to calls are defined. *)
let collect_argument_tids ~(target : T.Target.t)
    ~(func_info : Function_info.t)
    (blks : blk term list) : Tid.Set.t =
  let collect_func_args def (remaining, acc) =
    let lhs = Def.lhs def in
    if Var.Set.mem remaining lhs then
      let new_remaining = Var.Set.remove remaining lhs in
      let new_acc = Tid.Set.add acc (Term.tid def) in
      (new_remaining, new_acc)
    else (remaining, acc)
  in
  let get_func_args acc args blk =
    let defs = Term.enum def_t blk |> Seq.to_list in
    let partitioned_vars = List.fold_right defs
      ~init:(Var.Set.of_list args, acc)
      ~f:collect_func_args
    in
    snd partitioned_vars
  in
  let aux ~func_info acc blk =
    let args = collect_args blk ~target ~func_info in
    Option.value_map args
      ~default:acc
      ~f:(fun args -> get_func_args acc args blk)
  in
  List.fold blks
    ~init:Tid.Set.empty
    ~f:(fun acc blk -> aux acc blk ~func_info)

(* Create a fake memory assignment which is a signpost to the selector
   that the most recently assigned memory is a dependency (or "argument")
   of the function call. *)
let insert_new_mems_at_callsites (target : T.target)
    (blks : blk term list) : (blk term list * Tid.Set.t) =
  let mem = Var.reify (T.Target.data target) in
  let blks, tids = List.fold blks ~init:([], Tid.Set.empty)
    ~f:(fun (blks, tids) blk ->
      if Bir_helpers.has_call blk then
        let tid = Tid.create () in
        let lhs = Var.create (Var.name mem ^ "_call") (Var.typ mem) in
        let def = Def.create ~tid lhs @@ Var mem in
        let blk = Term.append def_t blk def in
        blk :: blks, Tid.Set.add tids tid
      else (blk :: blks, tids))
  in
  List.rev blks, tids

(* We shouldn't do any spilling if there are higher vars that depend
     on SP-relative locations in memory. *)
let check_hvars_for_existing_stack_locations
    (sp : var) (hvars : Hvar.t list) : (unit, Err.t) result =
  let- _ = Result.all (List.map hvars ~f:(fun hvar ->
      match Hvar.value hvar with
      | Hvar.(Memory (Frame (reg, offset))) ->
        if String.(reg <> Var.name sp) then Ok ()
        else 
          let msg = Format.sprintf
            "Existing stack location [%s, %s] used by hvar '%s'"
            reg (Word.to_string offset) (Hvar.name hvar) in
          Error (Types.Stack_loc_already_used msg)
      | _ -> Ok ())) in
  Ok ()

module Spill = struct
  (* [bir] - The transformed patch code.
     [hvars] - The updated higher vars info, after spilling occurred.
     [spilled] - The set of spilled variables.
  *)
  type t = {
    blks : blk term list;
    hvars : Hvar.t list;
    spilled : String.Set.t;
  }

  let collect_caller_save (target : T.target)
      ~(width : int) ~(stride : word) : (word * var) String.Map.t =
    (* Use predetermined stack locations. *)
    T.Target.regs target ~roles:T.Role.Register.[caller_saved] |>
    Set.to_list |> List.mapi ~f:(fun i v ->
        let idx = Word.of_int ~width i in
        let v = Var.reify v in
        let name = Var.name v in
        name, (Word.(stride * idx), v)) |>
    String.Map.of_alist_exn

  (* Find out which higher vars we need to spill. *)
  let spill_hvars
      (hvars : Hvar.t list)
      ~(preserved : String.Set.t ref)
      ~(restored : String.Set.t ref)
      ~(spilled : String.Set.t ref)
      ~(live_after_call : string -> bool)
      ~(caller_save : (word * var) String.Map.t)
      ~(sp : var) : (Hvar.t list, Err.t) result =
    let spill ?(restore = false) name v offset hvar =
      preserved := Set.add !preserved v;
      if restore then restored := Set.add !restored v;
      (* If it needs to be preserved, but is not live after a call,
         then we can still refer to it by register instead of by stack
         location. *)
      if restore || live_after_call name then begin
        let memory = Hvar.create_frame (Var.name sp) offset in
        spilled := Set.add !spilled name;
        Hvar.create_with_memory name ~memory
      end else hvar in
    let- result = Result.all (List.map hvars ~f:(fun hvar ->
        let name = Hvar.name hvar in
        match Hvar.value hvar with
        | Hvar.Registers {at_entry = Some v; at_exit} -> begin
            match Map.find caller_save v with
            | None -> Ok hvar
            | Some (offset, _) ->
              match at_exit with
              | Some v' when String.(v = v') ->
                Ok (spill name v offset hvar ~restore:true)
              | Some _ ->
                let msg = Format.sprintf
                  "Unexpected value for `at_exit` of higher var '%s'" name in
                Error (Types.Bad_hvar_at_exit msg)
              | None ->
                (* Don't bother to preserve the register if it's not live
                   after a call. *)
                if live_after_call name
                then Ok (spill name v offset hvar)
                else Ok hvar
          end
        | _ -> Ok hvar)) in
      Ok result

  (* Transform the code to create a stack frame based on the higher vars that
     were spilled. *)
  let create_activation_record
      (blks : blk term list)
      (hvars : Hvar.t list)
      ~(preserved : String.Set.t)
      ~(restored : String.Set.t)
      ~(spilled : String.Set.t)
      ~(caller_save : (word * var) String.Map.t)
      ~(sp : var)
      ~(mem : var)
      ~(endian : endian)
      ~(entry_blk : blk term)
      ~(space : word) : (t, Err.t) result =
    (* Place a register into a stack location. *)
    let push v =
      let open Bil.Types in
      let off, reg = Map.find_exn caller_save v in
      let reg = Naming.mark_reg reg in
      let addr = BinOp (PLUS, Var sp, Int off) in
      let tid = Tid.create () in
      Def.create ~tid mem @@ Store (Var mem, addr, Var reg, endian, `r32) in
    (* Load a register from a stack location. *)
    let pop v =
      let open Bil.Types in
      let off, reg = Map.find_exn caller_save v in
      let reg = Naming.mark_reg reg in
      let addr = BinOp (PLUS, Var sp, Int off) in
      let tid = Tid.create () in
      Def.create ~tid reg @@ Load (Var mem, addr, endian, `r32) in
    (* Create the new defs. *)
    let pushes =
      Set.to_list preserved |> List.rev |> List.map ~f:push in
    let pops = Set.to_list restored |> List.rev |> List.map ~f:pop in
    (* Insert the new defs into the entry/exit blocks accordingly. *)
    let- exit_tids =
      let- exits = Bir_helpers.exit_blks blks in
      Ok (List.map exits ~f:Term.tid |> Tid.Set.of_list) in
    let blks = List.map blks ~f:(fun blk ->
        let tid = Term.tid blk in
        if Tid.(tid = Term.tid entry_blk) then
          let blk = List.fold pushes ~init:blk ~f:(fun blk def ->
              let def = Term.set_attr def Bir_helpers.spill_tag () in
              Term.prepend def_t blk def) in
          let tid = Tid.create () in
          let adj = Def.create ~tid sp @@ BinOp (MINUS, Var sp, Int space) in
          Term.prepend def_t blk adj
        else if Set.mem exit_tids tid then
          let blk = List.fold pops ~init:blk ~f:(fun blk def ->
              let def = Term.set_attr def Bir_helpers.spill_tag () in
              Term.append def_t blk def) in
          let tid = Tid.create () in
          let adj = Def.create ~tid sp @@ BinOp (PLUS, Var sp, Int space) in
          Term.append def_t blk adj
        else blk) in
    Ok {blks; hvars; spilled}

  (* Spill higher vars in caller-save registers if we are doing any calls
     in a multi-block patch, since the ABI says they may be clobbered. *)
  let spill_hvars_and_adjust_stack
      (blks : blk term list)
      ~(target : T.target)
      ~(sp_align : int)
      ~(hvars : Hvar.t list)
      ~(entry_blk : blk term) : (t, Err.t) result =
    let calls = Bir_helpers.call_blks blks in
    if List.is_empty calls || List.length blks = 1
    then Ok {blks; hvars; spilled = String.Set.empty}
    else
      (* Collect the liveness information. *)
      let live =
        let sub = Bir_helpers.create_sub blks in
        Live.compute sub in
      (* Returns true if the variable is live after a call. *)
      let live_after_call v = List.exists calls ~f:(fun tid ->
          let out = Live.outs live tid in
          Set.exists out ~f:(fun v' -> String.(v = Var.name v'))) in
      let width = T.Target.bits target in
      let stride = Word.of_int ~width (width lsr 3) in
      let caller_save = collect_caller_save target ~width ~stride in
      let- sp =
        match T.Target.reg target T.Role.Register.stack_pointer with
        | Some v -> Ok (Var.reify v)
        | None ->
          let msg = Format.sprintf
            "No stack pointer register for target '%s'"
            (T.Target.to_string target) in
          Error (Types.No_SP msg) in
      (* Preserved and restored registers. *)
      let preserved = ref String.Set.empty in
      let restored = ref String.Set.empty in
      (* Variables that were spilled. *)
      let spilled = ref String.Set.empty in
      (* Find which higher vars to spill. *)
      let- hvars' = spill_hvars hvars
          ~preserved ~restored ~spilled
          ~live_after_call ~caller_save ~sp in
      (* If we needed to spill or restore, then make sure that other higher
         vars aren't using stack locations. *)
      let no_regs = Set.is_empty !preserved && Set.is_empty !restored in
      let- () =
        if no_regs then Ok ()
        else check_hvars_for_existing_stack_locations sp hvars in
      (* Do we need to change anything? *)
      if no_regs && sp_align = 0
      then Ok {blks; hvars = hvars'; spilled = !spilled}
      else
        let mem = Var.reify @@ T.Target.data target in
        let endian =
          if T.(Endianness.(Target.endianness target = eb))
          then BigEndian else LittleEndian in
        (* Predetermined amount of space to allocate on the stack. *)
        let space =
          Map.length caller_save * (width lsr 3) |>
          Int.round_up ~to_multiple_of:(T.Target.data_alignment target) in
        let space = Word.of_int ~width @@
          if no_regs then sp_align else sp_align + space in
        let sp = Naming.mark_reg sp in
        create_activation_record blks hvars'
          ~preserved:!preserved ~restored:!restored ~spilled:!spilled
          ~caller_save ~sp ~mem ~endian ~entry_blk ~space

end
