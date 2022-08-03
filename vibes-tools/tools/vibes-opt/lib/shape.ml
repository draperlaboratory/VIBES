open Core
open Bap.Std
open Graphlib.Std

module T = Bap_core_theory.Theory
module Err = Vibes_error_lib.Std
module Patch_info = Vibes_patch_info_lib.Types
module Bir_helpers = Vibes_bir_lib.Helpers

open Vibes_error_lib.Let

(* Remove any unreachable blocks. *)
let remove_unreachable
    (blks : blk term list)
    (entry_tid : tid) : blk term list =
  let module G = Graphs.Tid in
  let sub = Bir_helpers.create_sub blks in
  let cfg = Sub.to_graph sub in
  List.filter blks ~f:(fun blk ->
    let tid = Term.tid blk in
    Tid.(tid = entry_tid) ||
      let preds =
        G.Node.preds tid cfg |> Seq.to_list |> Tid.Set.of_list in
      not @@ Tid.Set.(is_empty @@ remove preds G.start))

(* We need to insert explicit exit blocks when we encounter the following
   kinds of jmps:
   - call <x> with noreturn
   - call <x> with return <indirect>
   - no jmps at all
   With noreturn calls and no jmps at all, we can have them share the
   same exit block at the very end of the program.
   With return <indirect>, we need to make unique exit blocks for each
   indirect target. *)
let adjust_exits (blks : blk term list) : (blk term list, Err.t) result =
  match blks with
  | [blk] when Bir_helpers.is_implicit_exit blk ->
    (* We're already in an acceptable form. Adding an extra block
       could lose us opportunity for peephole optimization. *)
    Ok blks
  | _ ->
    let- exit_tids =
      let- exits = Bir_helpers.exit_blks blks in
      let tids = List.map exits ~f:Term.tid in
      Ok (Tid.Set.of_list tids)
    in
    (* This is the block that we may insert as a continuation
       for `call ... with noreturn`, as well as blocks with no jumps
       at all. Since such blocks have no particular successor in the
       CFG, this block can be shared between them. *)
    let extra = ref None in
    let make_extra () = match !extra with
      | Some blk -> Term.tid blk
      | None ->
        let tid = Tid.create () in
        extra := Some (Blk.create ~tid ());
        tid
    in
    (* Returns to indirect targets, on the other hand, require us
       to make a unique block for each target. *)
    let extra_indirect = ref [] in
    (* Collect information about jmps that need to be rewrtitten. *)
    let blks = List.map blks ~f:(fun blk ->
        let tid = Term.tid blk in
        if Set.mem exit_tids tid then
          match Term.enum jmp_t blk |> Seq.to_list with
          | [] ->
            (* The block has no jumps, so it is implicitly an exit node.
               Make this explicitly jump to a new exit block at the very
               end of the program. *)
            let jmp_tid = Tid.create () in
            let tid' = make_extra () in
            let jmp = Jmp.create_goto ~tid:jmp_tid (Direct tid') in
            let defs = Term.enum def_t blk |> Seq.to_list in
            Blk.create ~tid ~jmps:[jmp] ~defs ()
          | [jmp] when not @@ Bir_helpers.is_unconditional jmp ->
            (* The block has a single conditional jump, so it is implicitly
               an exit node. Make this explicitly jump to a new exit block
               at the very end of the program. *)
            let jmp_tid = Tid.create () in
            let tid' = make_extra () in
            let jmp' = Jmp.create_goto ~tid:jmp_tid (Direct tid') in
            let defs = Term.enum def_t blk |> Seq.to_list in
            Blk.create ~tid ~jmps:[jmp; jmp'] ~defs ()
          | jmps ->
            let jmps = List.map jmps ~f:(fun jmp ->
                match Jmp.kind jmp with
                | Call call -> begin
                    match Call.return call with
                    | Some (Direct _) -> jmp
                    | Some (Indirect _ as label) ->
                      (* We need a unique exit node that has an indirect
                         `goto label`. *)
                      let tid = Tid.create () in
                      let blk =
                        Blk.create ~tid ~jmps:[Jmp.create_goto label] () in
                      extra_indirect := blk :: !extra_indirect;
                      Jmp.(with_dst jmp @@ Some (resolved tid))
                    | None ->
                      (* Calls should not be noreturn. Since BAP didn't give
                         this any particular target to return to, we can
                         have it return to a common exit block. *)
                      let tid = make_extra () in
                      Jmp.(with_dst jmp @@ Some (resolved tid))
                  end
                | _ -> jmp) in
            let defs = Term.enum def_t blk |> Seq.to_list in
            Blk.create ~tid ~defs ~jmps ()
        else blk) in
    (* Append the generated blocks. *)
    let extra = Option.value_map !extra ~default:[] ~f:List.return in
    Ok (blks @ !extra_indirect @ extra)

(* Order the blocks according to a reverse postorder DFS traversal.
   This should minimize the number of extra jumps we need to insert. *)
let reorder_blks (blks : blk term list) : blk term list =
  let sub = Bir_helpers.create_sub blks in
  let cfg = Sub.to_cfg sub in
  let blks =
    Graphlib.reverse_postorder_traverse (module Graphs.Ir) cfg |>
    Seq.to_list |> List.map ~f:Graphs.Ir.Node.label in
  (* The exit block with no jmps should be ordered at the very end of
     the program. *)
  match List.find blks ~f:Bir_helpers.is_implicit_exit with
  | None -> blks
  | Some blk ->
    let tid = Term.tid blk in
    let blks = List.filter blks ~f:(fun blk' ->
        Tid.(tid <> Term.tid blk')) in
    blks @ [blk]

(* Get the maximum address of each patch site. *)
let collect_conservative_patch_points
    ~(patch_info : Patch_info.t) ~(width : int) : word list =
  let patch_spaces =
    match Patch_info.patch_spaces patch_info with
    | None ->
      (* Fall back to the default patch point. *)
      let patch_point =
        let word = Patch_info.patch_point patch_info in
        let bv = Word.to_bitvec word in
        Bitvec.to_int64 bv in
      let patch_size = Patch_info.patch_size patch_info in
      [patch_point, patch_size]
    | Some spaces ->
      List.map spaces ~f:(fun space ->
          (Patch_info.address space, Patch_info.size space)) in
  List.map patch_spaces ~f:(fun (offset, size) ->
      Word.of_int64 ~width Int64.((offset + size) - 4L))

(* This is only meant for when we generate Thumb code, where the allowed
   range of conditional branches is limited (it is quite wide on ARM).
   We make a conservative estimate of how far these jumps are going to be,
   and if they are over a certain limit (dictated by the instruction set
   manual), then we need to break apart the jump into several blocks.

   Before (assume 0x123456 is too far away):

   bcc 0x123456
   b continue

   After:

   bcc relax
   b continue
   relax:
   b 0x123456

   It's important to run this after the edge contraction optimization,
   since it will undo any kind of branch relaxation at the BIR level. *)
let relax_branches
    (blks : blk term list)
    ~(target : T.Target.t)
    ~(patch_info : Patch_info.t) : blk term list =
  let width = T.Target.code_addr_size target in
  let patch_points =
    collect_conservative_patch_points ~patch_info ~width in
  let fwd_limit = Word.of_int ~width 0xFFFFE in
  let bwd_limit = Word.of_int ~width 0x100000 in
  let inserted = ref [] in
  let table = Addr.Table.create () in
  let can_fit addr = List.exists patch_points ~f:(fun maximum ->
      if Word.(maximum > addr) then
        Word.((maximum - addr) <= bwd_limit)
      else if Word.(addr > maximum) then
        Word.((addr - maximum) <= fwd_limit)
      else true) in
  let blks = List.map blks ~f:(fun blk ->
      let jmps =
        Term.enum jmp_t blk |> Seq.to_list |>
        List.map ~f:(fun jmp ->
            if Bir_helpers.is_unconditional jmp then jmp
            else match Jmp.kind jmp with
              | Goto (Indirect (Int addr)) as kind -> begin
                  match Addr.Table.find table addr with
                  | Some tid ->
                    Jmp.with_kind jmp @@ Goto (Direct tid)
                  | None when can_fit addr -> jmp
                  | None ->
                    let blk_tid = Tid.create () in
                    let jmp_tid = Tid.create () in
                    let new_jmp = Jmp.create kind ~tid:jmp_tid in
                    let new_blk = Blk.create () ~jmps:[new_jmp] ~tid:blk_tid in
                    inserted := new_blk :: !inserted;
                    Addr.Table.set table ~key:addr ~data:blk_tid;
                    Jmp.with_kind jmp @@ Goto (Direct blk_tid)
                end
              | _ -> jmp) in
      let defs = Term.enum def_t blk |> Seq.to_list in
      let tid = Term.tid blk in
      Blk.create () ~tid ~defs ~jmps) in
  blks @ !inserted

(* This is only meant for when we generate Thumb code.

   Example:

   x := a + 1
   y := z
   when x < z goto %00001234

   Gets compiled to:

   movs x, a
   adds x, 1
   movs y, z
   cmp x, z
   bls blk00001234
 
   Many simple instructions (data transfer, arithmetic) on Thumb will
   set the flags if they have a two-byte encoding. Unfortunately, we
   don't currently model these effects on the flags in the instruction
   selector (or our Minizinc model), so the scheduler might actually
   come up with this arrangement:

   movs x, a
   adds x, 1
   cmp x, y
   movs y, z
   bls blk00001234

   The `movs y, z` has now clobbered the flags and will change the
   result of the branch taken. This pass will transform the code to:

   movs x, a
   adds x, 1
   movs y, z
   b split
   split:
   cmp x, z
   bls blk00001234

   Here, the block `split` exists only to evaluate the condition and
   then perform the branch. Later we can remove the `b split` instruction
   in the peephole optimizer since we can replace it with a fallthrough.
   By making this control transfer explicit we prevent our Minizinc model
   from changing the ordering of the comparison instruction. *)
let split_on_conditional (blks : blk term list) : blk term list =
  let rec go ?(split = Tid.Set.empty) acc = function
    | [] -> List.rev acc
    | b :: bs ->
      let b, bs, split =
        if Set.mem split @@ Term.tid b then
          (b, bs, split)
        else
          let jmps = Term.enum jmp_t b |> Seq.to_list in
          if List.exists jmps ~f:(Fn.non Bir_helpers.is_unconditional) then
            let jmp_tid = Tid.create () in
            let blk_tid = Tid.create () in
            let jmp = Jmp.create ~tid:jmp_tid @@ Goto (Direct blk_tid) in
            let b = Blk.create ()
                ~tid:(Term.tid b)
                ~jmps:[jmp]
                ~defs:(Term.enum def_t b |> Seq.to_list)
                ~phis:(Term.enum phi_t b |> Seq.to_list) in
            let b' = Blk.create () ~tid:blk_tid ~jmps in
            b, b' :: bs, Set.add split blk_tid
          else (b, bs, split) in
      go (b :: acc) bs ~split in
  go [] blks
