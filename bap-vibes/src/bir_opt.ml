(* Implements {!Bir_opt}. *)

open !Core_kernel
open Bap.Std

type opt = blk term list -> blk term list

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
let apply_list (opts : opt list) ir =
  List.fold ~init:ir
    ~f:(fun current_ir opt -> opt current_ir)
    opts

let apply (ir : blk term list) : blk term list =
  (* This list contains all the optimizations we currently apply *)
  let opts = [short_circ] in
  apply_list opts ir
