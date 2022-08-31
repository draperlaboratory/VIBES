open Core
open Bap_core_theory

module T = Theory
module Log = Vibes_log.Stream
module Ir = Vibes_ir.Types
module Params = Vibes_minizinc.Types.Params
module Solution = Vibes_minizinc.Types.Solution
module Minizinc = Vibes_minizinc.Utils

let (let*) x f = Result.bind x ~f

let opt
    (ir : Ir.t)
    (target : T.target) : (Ir.t, KB.conflict) result =
  let* is_nop, unconditional_branch_target =
    if T.Target.belongs Arm_target.parent target then
      Ok Arm_utils.(is_nop, unconditional_branch_target)
    else
      let msg = Format.asprintf
          "Unsupported target %a"
          T.Target.pp target in
      Error (Errors.Unsupported_target msg) in
  Ok (Opt.peephole ir ~is_nop ~unconditional_branch_target)

let solve
    (ir : Ir.t)
    (target : T.target)
    (language : T.language)
    (model_filepath : string) : (Ir.t, KB.conflict) result =
  let* params, info = Params.serialize ir target language in
  let* solution_filepath = Minizinc.run_minizinc params ~model_filepath in
  let* solution = Solution.deserialize solution_filepath info in
  let ir = Solution.apply ir solution in
  let* ir = opt ir target in
  Ok ir
