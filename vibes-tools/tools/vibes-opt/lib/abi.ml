open Core
open Bap.Std
open Bap_core_theory

module T = Theory
module Log = Vibes_log.Stream
module Function_info = Vibes_function_info.Types
module Hvar = Vibes_higher_vars.Higher_var
module Naming = Vibes_higher_vars.Substituter.Naming
module Bir_helpers = Vibes_bir.Helpers

open KB.Syntax

(* Reify the argument names into variables. *)
let vars_of_args
    (args : string list)
    ~(target : T.target) : var list =
  let s = T.Bitv.define @@ T.Target.bits target in
  List.map args ~f:(fun arg -> Var.reify @@ T.Var.define s arg)

(* Collect the args to the call, if any. *)
let collect_args
    (blk : blk term)
    ~(target : T.target)
    ~(func_info : Function_info.t) : var list option KB.t =
  let match_jmp jmp = match Jmp.alt jmp with
    | None -> !!None
    | Some dst -> match Jmp.resolve dst with
      | Second _ -> !!None
      | First tid ->
        (* Only direct calls are applicable. *)
        let+ args = Function_info.find_args func_info ~tid in
        Option.map args ~f:(vars_of_args ~target) in
  Term.enum jmp_t blk |> KB.Seq.find_map ~f:match_jmp

(* Mark the tids where the arguments to calls are defined. *)
let mark_argument_tids
    (sub : sub term)
    ~(target : T.target)
    ~(func_info : Function_info.t) : sub term KB.t =
  let collect_func_args def (remaining, acc) =
    let lhs = Def.lhs def in
    if Var.Set.mem remaining lhs then
      let new_remaining = Var.Set.remove remaining lhs in
      let new_acc = Tid.Set.add acc @@ Term.tid def in
      new_remaining, new_acc
    else remaining, acc in
  let func_args blk args =
    Term.enum def_t blk |> Seq.to_list |> List.fold_right
      ~init:(Var.Set.of_list args, Tid.Set.empty)
      ~f:collect_func_args |> snd in
  Term.KB.map blk_t sub ~f:(fun blk ->
      let+ args = collect_args blk ~target ~func_info in
      Option.value_map args ~default:blk ~f:(fun args ->
          let tids = func_args blk args in
          Term.map def_t blk ~f:(fun def ->
              if Set.mem tids @@ Term.tid def then
                Term.set_attr def Bir_helpers.argument_tag ()
              else def)))

(* Create a fake memory assignment which is a signpost to the selector
   that the most recently assigned memory is a dependency (or "argument")
   of the function call. *)
let insert_new_mems_at_callsites
    (sub : sub term)
    ~(target : T.target) : sub term KB.t =
  let mem = Var.reify @@ T.Target.data target in
  Term.KB.map blk_t sub ~f:(fun blk ->
      if Bir_helpers.has_call blk then
        let+ tid = T.Label.fresh in
        let name = Format.sprintf "%s:call" @@ Var.name mem in
        let lhs = T.Var.define (Var.sort mem) name in
        let def = Def.create ~tid (Var.reify lhs) @@ Var mem in
        Term.append def_t blk @@
        Term.set_attr def Bir_helpers.argument_tag ()
      else !!blk)

(* We shouldn't do any spilling if there are higher vars that depend
   on SP-relative locations in memory. *)
let check_hvars_for_existing_stack_locations
    (sp : var)
    (hvars : Hvar.t list) : (unit, KB.conflict) result =
  let sp = Var.name sp in
  List.map hvars ~f:(fun hvar -> match hvar.value with
      | Memory (Frame (reg, offset)) when String.(reg = sp) ->
        let msg = Format.asprintf
            "Existing stack location [%s, %a] used by hvar '%s'"
            reg Word.pp offset hvar.name in
        Error (Errors.Stack_loc_already_used msg)
      | _ -> Ok ()) |>
  Result.all |> Result.map ~f:ignore

let collect_caller_save
    (target : T.target)
    ~(width : int)
    ~(stride : word) : (word * var) String.Map.t =
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
    ~(live_after_call : string -> bool)
    ~(caller_save : (word * var) String.Map.t)
    ~(sp : var) : (Hvar.t list, KB.conflict) result =
  let spill ?(restore = false) name v offset hvar =
    preserved := Set.add !preserved v;
    if restore then restored := Set.add !restored v;
    (* If it needs to be preserved, but is not live after a call,
       then we can still refer to it by register instead of by
       stack location. *)
    if restore || live_after_call name then begin
      let memory = Hvar.Frame (Var.name sp, offset) in
      Hvar.{name; value = Memory memory}
    end else hvar in
  List.map hvars ~f:(fun ({name; value} as hvar) -> match value with
      | Hvar.Registers {at_entry = Some entry; at_exit; _} -> begin
          match Map.find caller_save entry with
          | None -> Ok hvar
          | Some (offset, _) ->
            match at_exit with
            | Some exit when String.(entry = exit) ->
              Ok (spill name entry offset hvar ~restore:true)
            | Some _ ->
              let msg = Format.sprintf
                  "Unexpected value for `at_exit` \
                   of higher var '%s'" name in
              Error (Errors.Bad_hvar_at_exit msg)
            | None ->
              (* Don't bother to preserve the register if it's
                 not live after a call. *)
              if live_after_call name
              then Ok (spill name entry offset hvar)
              else Ok hvar
        end
      | _ -> Ok hvar) |>
  Result.all |> Result.map ~f:Fn.id

let liftr : ('a, KB.conflict) result -> 'a KB.t = function
  | Error err -> KB.fail err
  | Ok x -> !!x

(* Transform the code to create a stack frame based on the higher vars that
   were spilled. *)
let create_activation_record
    (sub : sub term)
    (hvars : Hvar.t list)
    ~(target : T.target)
    ~(preserved : String.Set.t)
    ~(restored : String.Set.t)
    ~(caller_save : (word * var) String.Map.t)
    ~(sp : var)
    ~(mem : var)
    ~(endian : endian)
    ~(space : word) : (sub term * Hvar.t list) KB.t =
  (* Insert the new defs into the entry/exit blocks accordingly. *)
  let* exits = liftr @@ Bir_helpers.exit_blks sub in
  let* entry_tid = liftr @@ Bir_helpers.entry_tid sub in
  let size = Size.of_int_exn @@ T.Target.data_addr_size target in
  (* Create the new defs. *)
  let* pushes =
    Set.to_list preserved |> List.rev |>
    KB.List.map ~f:(fun v ->
        let off, reg = Map.find_exn caller_save v in
        let reg = Naming.mark_reg_unsafe reg in
        let+ tid = T.Label.fresh in
        Def.create ~tid mem Bil.(
            store ~mem:(var mem) ~addr:(var sp + int off)
              (var reg) endian size)) in
  let* pops =
    Set.to_list restored |> List.rev |>
    KB.List.map ~f:(fun v ->
        let off, reg = Map.find_exn caller_save v in
        let reg = Naming.mark_reg_unsafe reg in
        let+ tid = T.Label.fresh in
        Def.create ~tid reg Bil.(
            load ~mem:(var mem) ~addr:(var sp + int off)
              endian size)) in
  let exit_tids = List.map exits ~f:Term.tid |> Tid.Set.of_list in
  let+ sub = Term.KB.map blk_t sub ~f:(fun blk ->
      let tid = Term.tid blk in
      if Tid.(tid = entry_tid) then
        let blk = List.fold pushes ~init:blk ~f:(fun blk def ->
            let def = Term.set_attr def Bir_helpers.spill_tag () in
            Term.prepend def_t blk def) in
        let+ tid = T.Label.fresh in
        let adj = Def.create ~tid sp Bil.(var sp - int space) in
        Term.prepend def_t blk adj
      else if Set.mem exit_tids tid then
        let blk = List.fold pops ~init:blk ~f:(fun blk def ->
            let def = Term.set_attr def Bir_helpers.spill_tag () in
            Term.append def_t blk def) in
        let+ tid = T.Label.fresh in
        let adj = Def.create ~tid sp Bil.(var sp + int space) in
        Term.append def_t blk adj
      else !!blk) in
  sub, hvars

(* Spill higher vars in caller-save registers if we are doing any calls
   in a multi-block patch, since the ABI says they may be clobbered. *)
let spill_hvars_and_adjust_stack
    (sub : sub term)
    ~(target : T.target)
    ~(sp_align : int)
    ~(hvars : Hvar.t list) : (sub term * Hvar.t list) KB.t =
  let calls = Bir_helpers.call_blks sub in
  if List.is_empty calls || Term.length blk_t sub = 1
  then !!(sub, hvars)
  else
    (* Collect the liveness information. *)
    let live = Live.compute sub in
    (* Returns true if the variable is live after a call. *)
    let live_after_call name = List.exists calls ~f:(fun blk ->
        let out = Live.outs live @@ Term.tid blk in
        Set.exists out ~f:(fun v -> String.(name = Var.name v))) in
    let width = T.Target.bits target in
    let stride = Word.of_int ~width (width lsr 3) in
    let caller_save = collect_caller_save target ~width ~stride in
    let* sp = match T.Target.reg target T.Role.Register.stack_pointer with
      | Some v -> !!(Var.reify v)
      | None ->
        let msg = Format.sprintf
            "No stack pointer register for target '%s'"
            (T.Target.to_string target) in
        KB.fail @@ Errors.No_SP msg in
    (* Preserved and restored registers. *)
    let preserved = ref String.Set.empty in
    let restored = ref String.Set.empty in
    (* Find which higher vars to spill. *)
    spill_hvars hvars ~live_after_call ~caller_save
      ~preserved ~restored ~sp |> function
    | Error err -> KB.fail err
    | Ok new_hvars ->
      (* If we needed to spill or restore, then make sure that other
         higher vars aren't using stack locations.

         TODO: Maybe instead of failing when this happens, we could
         change the offset to match how much we're going to be
         subtracting from the stack pointer.
      *)
      let no_regs =
        Set.is_empty !preserved &&
        Set.is_empty !restored in
      let* () =
        if not no_regs then
          match check_hvars_for_existing_stack_locations sp hvars with
          | Error err -> KB.fail err
          | Ok () -> !!()
        else !!() in
      (* Do we need to change anything? *)
      if no_regs && sp_align = 0
      then !!(sub, new_hvars)
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
        let sp = Naming.mark_reg_unsafe sp in
        create_activation_record sub new_hvars
          ~preserved:!preserved
          ~restored:!restored
          ~caller_save
          ~sp
          ~mem
          ~endian
          ~space
          ~target
