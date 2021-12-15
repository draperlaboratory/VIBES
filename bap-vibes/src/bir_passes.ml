(* Implements {!Bir_opt}. *)

open !Core_kernel
open Bap.Std
open Bap_core_theory
open Graphlib.Std
open KB.Let

module Arm = Arm_selector
module Subst = Substituter
module Hvar = Higher_var

type t = {
  ir : blk term list;
  exclude_regs : String.Set.t;
}

(* Helper functions *)
module Helper = struct

  (* Helper functions *)
  let new_tmp (width : int) : var =
    Var.create ~is_virtual:true ~fresh:true "tmp" (Type.Imm width)

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

  (* Returns true if there are calls in the patch *)
  let has_calls (blks : blk term list) : bool =
    List.exists blks ~f:(fun blk ->
        Term.enum jmp_t blk |> Seq.exists ~f:(fun jmp ->
            match Jmp.kind jmp with
            | Call _ -> true
            | _ -> false))

end

(* Optimizations *)
module Opt = struct

  type t = blk term list -> blk term list

  (* Returns the block that is the destination of the jump, if it is in
     the supplied list, and the jump is direct. returns [None]
     otherwise. *)
  let find_tgt blks jmp : blk term option =
    let dst = Jmp.dst jmp in
    match Option.map ~f:Jmp.resolve dst with
    | Some (First tid) ->
      List.find blks ~f:(fun blk -> Tid.(Term.tid blk = tid))
    | _ -> None

  (* If the input block is a single goto statement, returns the
     destination of that jump. Returns [None] otherwise. *)
  let is_redirect blk : Jmp.dst option =
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
  let short_circ_jmp blks jmp : jmp term option =
    match find_tgt blks jmp with
    | None -> None
    | Some blk ->
      begin
        match is_redirect blk with
        | None -> None
        | Some dst ->
          Some (Jmp.with_dst jmp (Some dst))
      end

  let short_circ_blk blks blk : blk term =
    Term.map jmp_t blk
      ~f:(fun jmp ->
          match short_circ_jmp blks jmp with
          | None -> jmp
          | Some jmp' -> jmp')

  let short_circ blks =
    List.map blks ~f:(short_circ_blk blks)

  (* Applies all the optimizations in the list *)
  let apply_list (opts : t list) ir =
    List.fold ~init:ir
      ~f:(fun current_ir opt -> opt current_ir)
      opts

  let apply (ir : blk term list) : blk term list =
    (* This list contains all the optimizations we currently apply *)
    let opts = [short_circ] in
    apply_list opts ir

  (* Attempt to merge adjacent blocks which have an edge in between them.
     For this transformation, the blks must be ordered  according to a
     reverse postorder DFS traversal. *)
  let merge_adjacent (ir : blk term list) : blk term list =
    let rec aux acc = function
      | [] -> List.rev acc
      | [blk] -> List.rev (blk :: acc)
      | blk1 :: blk2 :: rest -> begin
          let jmps1 = Term.enum jmp_t blk1 |> Seq.to_list in
          match jmps1 with
          | [jmp] -> begin
              match Jmp.kind jmp with
              | Goto (Direct tid) when Tid.(tid = Term.tid blk2) ->
                let defs1 = Term.enum def_t blk1 |> Seq.to_list in
                let defs2 = Term.enum def_t blk2 |> Seq.to_list in
                let jmps2 = Term.enum jmp_t blk2 |> Seq.to_list in
                let new_blk =
                  Blk.create ~defs:(defs1 @ defs2) ~jmps:jmps2
                    ~tid:(Term.tid blk1) () in
                aux acc (new_blk :: rest)
              | _ -> aux (blk2 :: blk1 :: acc) rest
            end
          | _ -> aux (blk2 :: blk1 :: acc) rest
        end
    in
    aux [] ir

end

(* Change the shape of the code for the instruction selector. *)
module Shape = struct

  (* If there are exit nodes of the form `call ... with noreturn` or
     `call ... with return <indirect>`, then insert a new blk for them
     to return to. *)
  let adjust_noreturn_exits (blks : blk term list) : blk term list KB.t =
    let* exits = Helper.exit_blks blks in
    let exits = List.map exits ~f:Term.tid |> Tid.Set.of_list in
    let extra = ref None and extra_ind = ref [] in
    let tbl = Tid.Table.create () in
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
                    extra_ind := blk' :: !extra_ind;
                    Tid.Table.set tbl ~key:tid ~data:tid'
                  | None ->
                    let+ tid' = match !extra with
                      | Some blk' -> KB.return @@ Term.tid blk'
                      | None ->
                        let+ tid' = Theory.Label.fresh in
                        let blk' = Blk.create ~tid:tid' () in
                        extra := Some blk';
                        tid' in
                    Tid.Table.set tbl ~key:tid ~data:tid'
                end
              | _ -> KB.return ())
        else KB.return ()) in
    let blks = List.map blks ~f:(fun blk ->
        Term.map jmp_t blk ~f:(fun jmp ->
            let tid = Term.tid jmp in
            match Tid.Table.find tbl tid with
            | Some tid' -> Jmp.(with_dst jmp @@ Some (resolved tid'))
            | None -> jmp)) in
    let extra = Option.value_map !extra ~default:[] ~f:List.return in
    blks @ extra @ !extra_ind

  (* On Thumb, the selector will try to create a conditional `bl` instruction,
     which is illegal, so we need to insert a new block for the call, with
     a conditional goto to this new block. *)
  let massage_conditional_calls (blks : blk term list) : blk term list KB.t =
    let new_blks = ref [] in
    let kind = Tid.Table.create () in
    let+ () = KB.List.iter blks ~f:(fun blk ->
        Term.enum jmp_t blk |> KB.Seq.iter ~f:(fun jmp ->
            let tid = Term.tid jmp in
            match Jmp.kind jmp with
            | Call call when Exp.(Jmp.cond jmp <> Int Word.b1) ->
              let* tid' = Theory.Label.fresh in
              let+ tid'' = Theory.Label.fresh in
              let blk' = Blk.create ~tid:tid' ~jmps:[
                  Jmp.create_call ~tid:tid'' call;
                ] () in
              new_blks := blk' :: !new_blks;
              Tid.Table.set kind ~key:tid ~data:(Goto (Direct tid'))
            | _ -> KB.return ())) in
    let blks =
      List.map blks ~f:(Term.map jmp_t ~f:(fun jmp ->
          match Tid.Table.find kind @@ Term.tid jmp with
          | Some kind -> Jmp.with_kind jmp kind
          | None -> jmp)) in
    blks @ !new_blks

  (* Order the blocks according to a reverse postorder DFS traversal.
     This should minimize the number of extra jumps we need to insert. *)
  let reorder_blks (blks : blk term list) : blk term list KB.t =
    let+ sub = Helper.create_sub blks in
    let cfg = Sub.to_cfg sub in
    Graphlib.reverse_postorder_traverse (module Graphs.Ir) cfg |>
    Seq.to_list |> List.map ~f:Graphs.Ir.Node.label

end

(* ARM-specific transformations *)
module Arm_specific = struct

  (* Split the word into the lower and upper 16-bit halves. *)
  let split_word (w : word) : word * word * word =
    let shift = Word.of_int ~width:32 16 in
    let lower = Word.(w land of_int ~width:32 0xFFFF) in
    let upper = Word.(w lsr shift) in
    shift, lower, upper

  (* The particular pattern we're going to use as a signpost that a split
     happened. *)
  let idiom (v : var) (shift : word) (upper : word) : exp =
    Bil.(var v lor (int upper lsl int shift))

  (* Generic function that will split integers that are too large. *)
  let split
      (prev_tids : (tid * tid) option ref)
      (saved_temps : var Word.Table.t)
      (rhs : exp)
      ~(update : exp -> unit)
      ~(add : def term list -> unit) : unit KB.t =
    let+ tid1, tid2 = match !prev_tids with
      | Some tids -> KB.return tids
      | None ->
        let* tid1 = Theory.Label.fresh in
        let+ tid2 = Theory.Label.fresh in
        tid1, tid2 in
    prev_tids := Some (tid1, tid2);
    let rhs' = (object
      inherit Exp.mapper
      method! map_int w = match Word.Table.find saved_temps w with
        | Some v -> Var v
        | None ->
          if Word.to_int_exn w > 0xFFFF then 
            let shift, lower, upper = split_word w in
            let v = Helper.new_tmp 32 in
            let def1 = Def.create ~tid:tid1 v @@ Int lower in
            let def2 = Def.create ~tid:tid2 v @@ idiom v shift upper in
            Word.Table.set saved_temps ~key:w ~data:v;
            prev_tids := None;
            add [def2; def1];
            Var v
          else Int w
    end)#map_exp rhs in
    match !prev_tids with
    | None -> update rhs'
    | Some _ -> () 

  (* On ARM, we can't use constants larger than 65535. The typical idiom is
     to load the lower 16 bits first, then load the upper 16 bits, using a
     movw/movt idiom. *)
  let split_large_const (blks : blk term list) : blk term list KB.t =
    (* This is so we don't create fresh tids that are discareded. *)
    let prev_tids = ref None in
    KB.List.map blks ~f:(fun blk ->
        (* Mapping from constants to temporaries, just so we can cut down
           on the number of defs we create. *)
        let saved_temps = Word.Table.create () in
        let split = split prev_tids saved_temps in
        (* Save the results in these mutable structures. *)
        let prepend_defs = Tid.Table.create () in
        let rhs = Tid.Table.create () in
        let cond = Tid.Table.create () in
        let append_defs = ref [] in
        (* Handle defs *)
        let* () =
          Term.enum def_t blk |> KB.Seq.iter ~f:(fun def ->
              let tid = Term.tid def in
              match Def.rhs def with
              | Int w when Word.to_int_exn w > 0xFFFF -> begin
                  (* Special case for when the RHS is just an integer.
                     Here, we avoid using a temporary variable to hold
                     the results of the split. *)
                  match Word.Table.find saved_temps w with
                  | Some v -> KB.return @@
                    Tid.Table.set rhs ~key:tid ~data:(Bil.var v)
                  | None ->
                    let shift, lower, upper = split_word w in
                    let lhs = Def.lhs def in
                    let+ () =
                      let+ tid' = Theory.Label.fresh in
                      let def1 = Def.create ~tid:tid' lhs Bil.(int lower) in
                      Tid.Table.change prepend_defs tid ~f:(function
                          | None -> Some [def1]
                          | Some _ -> assert false) in
                    Tid.Table.set rhs ~key:tid ~data:(idiom lhs shift upper)
                end
              | exp ->
                split exp
                  ~update:(fun exp -> Tid.Table.set rhs ~key:tid ~data:exp)
                  ~add:(fun defs ->
                      Tid.Table.change prepend_defs tid ~f:(function
                          | None -> Some defs
                          | Some _ -> assert false))) in
        (* Handle conditions in jmps. We could probably also handle
           jmp destinations, but it would be tricky since they are
           assembled with PC-relative offsets as operands. *)
        let+ () =
          Term.enum jmp_t blk |> KB.Seq.iter ~f:(fun jmp ->
              let tid = Term.tid jmp in
              Jmp.cond jmp |> split
                ~update:(fun exp -> Tid.Table.set cond ~key:tid ~data:exp)
                ~add:(fun defs -> append_defs := !append_defs @ defs)) in
        (* Prepend new defs (from previous defs) *)
        let blk =
          Tid.Table.fold prepend_defs ~init:blk
            ~f:(fun ~key:tid ~data:defs blk ->
                List.fold defs ~init:(blk, tid) ~f:(fun (blk, before) def ->
                    Term.prepend def_t blk def ~before, Term.tid def) |> fst)
        in
        (* Set the new RHS. *)
        let blk =
          Term.map def_t blk ~f:(fun def ->
              match Tid.Table.find rhs @@ Term.tid def with
              | Some rhs -> Def.with_rhs def rhs
              | None -> def) in
        (* Set the new conds. *)
        let blk =
          Term.map jmp_t blk ~f:(fun jmp ->
              match Tid.Table.find cond @@ Term.tid jmp with
              | Some cond -> Jmp.with_cond jmp cond
              | None -> jmp) in
        (* Append new defs (at the end of the blk) *)
        List.fold !append_defs ~init:blk ~f:(fun blk def ->
            Term.append def_t blk def))

end

(* Handle storage of registers in the presence of ABI information. *)
module Registers = struct

  (* We shouldn't do any spilling if there are higher vars that depend
     on SP-relative locations in memory. *)
  let check_hvars_for_existing_stack_locations
      (sp : var) (hvars : Hvar.t list) : unit =
    List.iter hvars ~f:(fun hvar ->
        let value = Hvar.value hvar in
        match Hvar.at_entry value with
        | None -> ()
        | Some at_entry -> match Hvar.memory at_entry with
          | None -> ()
          | Some memory -> match Hvar.frame memory with
            | None -> ()
            | Some (reg, offset) ->
              if String.(reg = Var.name sp) then
                failwith @@ sprintf
                  "Existing stack location [%s, %s] used by hvar %s"
                  reg (Word.to_string offset) (Hvar.name hvar))

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
      let sp =
        match Theory.Target.reg tgt Theory.Role.Register.stack_pointer with
        | None -> failwith @@
          sprintf "No stack pointer register for target %s\n%!"
            (Theory.Target.to_string tgt)
        | Some v -> Var.reify v in
      let spilled = ref String.Set.empty
      and restored = ref String.Set.empty in
      (* Find which hvars we need to spill. *)
      let spill ?(restore = false) name v offset =
        let memory = Hvar.create_frame (Var.name sp) offset in
        let at_entry = Hvar.stored_in_memory memory in
        spilled := Set.add !spilled v;
        if restore then restored := Set.add !restored v;
        Hvar.create_with_storage name ~at_entry ~at_exit:None in
      let hvars' = List.map hvars ~f:(fun hvar ->
          let name = Hvar.name hvar in
          let value = Hvar.value hvar in
          match Hvar.at_entry value with
          | None -> hvar
          | Some at_entry -> match Hvar.register at_entry with
            | None -> hvar
            | Some v -> match Map.find caller_save v with
              | None -> hvar
              | Some (offset, _) -> match Hvar.at_exit value with
                | None -> spill name v offset
                | Some at_exit ->
                  match Hvar.register at_exit with
                  | Some v' when String.(v = v') ->
                    spill name v offset ~restore:true
                  | _ ->
                    (* XXX: handle the value being restored in a different
                       destination? *)
                    failwith @@ sprintf
                      "Unexpected value for `at_exit` of higher var %s"
                      name) in
      let* exits = Helper.exit_blks blks in
      let exits = List.map exits ~f:Term.tid |> Tid.Set.of_list in
      let no_regs = Set.is_empty !spilled && Set.is_empty !restored in
      if not no_regs then check_hvars_for_existing_stack_locations sp hvars;
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
        let push v =
          let open Bil.Types in
          let off, reg = Map.find_exn caller_save v in
          let addr = BinOp (PLUS, Var sp, Int off) in
          let+ tid = Theory.Label.fresh in
          Def.create ~tid mem @@ Store (Var mem, addr, Var reg, endian, `r32) in
        let pop v =
          let open Bil.Types in
          let off, reg = Map.find_exn caller_save v in
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
        let value = Hvar.value hvar in
        match Hvar.at_entry value with
        | None -> acc
        | Some at_entry ->
          match Hvar.at_exit value with
          | None -> acc
          | Some at_exit ->
            match Hvar.register at_entry, Hvar.register at_exit with
            | Some v, Some v'
              when String.(v = v') && Set.mem callee_save v ->
              String.Set.add acc v
            | _ -> acc)

end

(* VIBES IR requires linear SSA form. *)
let to_linear_ssa (blks : blk term list) : blk term list KB.t =
  let+ sub = Helper.create_sub blks in
  sub |> Sub.ssa |> Linear_ssa.transform |> Term.enum blk_t |> Seq.to_list

let create (code : insn)
    ~(tgt : Theory.target)
    ~(lang : Theory.language)
    ~(hvars : Hvar.t list)
    ~(sp_align : int) : t KB.t =
  let ir = Blk.from_insns [code] in
  (* BAP will give us the blks in such an order that the first one is the
     entry blk. *)
  let entry_blk = List.hd_exn ir in
  let* ir = Shape.adjust_noreturn_exits ir in
  let* ir, hvars =
    Registers.spill_hvars_and_adjust_stack tgt sp_align hvars entry_blk ir in
  let exclude_regs = Registers.collect_exclude_regs tgt hvars in
  let ir = Opt.apply ir in
  let* ir = Subst.substitute tgt hvars ir in
  let* ir =
    if Arm.is_arm_or_thumb lang
    then Arm_specific.split_large_const ir
    else KB.return ir in
  let* ir =
    if Arm.is_thumb lang
    then Shape.massage_conditional_calls ir
    else KB.return ir in
  let* ir = Shape.reorder_blks ir in
  let ir = Opt.merge_adjacent ir in
  let+ ir = to_linear_ssa ir in
  {ir; exclude_regs}

