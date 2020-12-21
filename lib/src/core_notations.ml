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
  (* Unsigned by default *)
  let (>>) a b = shiftr b0 a b
  let (<<) a b = shiftl b0 a b
  let (<) = slt
  let (<=) = sle
  let (>) = sgt
  let (>=) = sge
  let (!=) = neq

  let data_body l =
    let empty = perform Effect.Sort.bot in
    List.fold ~init:empty ~f:seq l

  let ctrl_body l =
    let empty = perform Effect.Sort.fall in
    List.fold ~init:empty ~f:seq l

end
