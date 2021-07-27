(* Implements {!Bir_opt}. *)

open !Core_kernel
open Bap.Std

type opt = blk term list -> blk term list

(* Applies all the optimizations in the list *)
let apply_list (opts : opt list) ir =
  List.fold ~init:ir
    ~f:(fun current_ir opt -> opt current_ir)
    opts

let apply ir =
  let opts = [ident] in
  apply_list opts ir
