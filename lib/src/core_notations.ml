(***********************************************************
 *
 * This module contains notational conveniences for Core
 * implementations. To use with a Core theory [C], just do
 * [open Core_notations.Make(C)].
 *
 * (You probably also want to do [open C] as well.)
 *
 *
 *
 *
 *************************************************************)
open !Core_kernel
open Bap_core_theory
open Theory

module Make (C : Core) = struct

  open C

  let ( := ) = set
  let (+) = add
  let (-) = sub
  let ( * ) = mul
  let (/) = sdiv
  let (mod) = smodulo
  let (&&) = and_
  let (&) = logand
  let (||) = or_
  let (|$) = logor
  let (~~) = not

  (* Shifts are unsigned by default *)
  let (>>) a b = shiftr b0 a b
  let (<<) a b = shiftl b0 a b

  let (<) = slt
  let (<=) = sle
  let (>) = sgt
  let (>=) = sge
  let (!=) = neq

  (** Sequences a list of data effects *)
  let data_body (l : data eff list) : data eff =
    let empty = perform Effect.Sort.bot in
    List.fold_right ~init:empty ~f:seq l

  (** Sequences a list of control effects, ending them with a
      fall-through *)
  let ctrl_body (l : ctrl eff list) : ctrl eff =
    let empty = perform Effect.Sort.fall in
    List.fold_right ~init:empty ~f:seq l

end
