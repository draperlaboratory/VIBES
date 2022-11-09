open Core
open Bap_core_theory

module T = Theory
module CT = Vibes_utils.Core_theory
module Log = Vibes_log.Stream
module Ir = Vibes_ir.Types
module Params = Vibes_minizinc.Types.Params
module Solution = Vibes_minizinc.Types.Solution
module Minizinc = Vibes_minizinc.Utils

let (let*) x f = Result.bind x ~f
let (let+) x f = Result.map x ~f

let opt
    (ir : Ir.t)
    (target : T.target) : (Ir.t, KB.conflict) result =
  let+ is_nop,
       unconditional_branch_target,
       is_move =
    if CT.is_arm32 target then
      let open Arm_utils in
      Ok (is_nop,
          unconditional_branch_target,
          is_move)
    else
      let msg = Format.asprintf
          "Unsupported target %a"
          T.Target.pp target in
      Error (Errors.Unsupported_target msg) in
  Opt.peephole ir
    ~is_nop
    ~unconditional_branch_target
    ~is_move

let solve
    ?(constraints : string option = None)
    (ir : Ir.t)
    (target : T.target)
    (language : T.language)
    (model_filepath : string) : (Ir.t, KB.conflict) result =
  let* params, info = Params.serialize ir target language in
  let model_filepath =
    Option.value_map constraints ~default:model_filepath
      ~f:(Minizinc.build_constraints_file ~model_filepath) in
  let* solution_filepath = Minizinc.run_minizinc params ~model_filepath in
  let* solution = Solution.deserialize solution_filepath info in
  let ir = Solution.apply ir solution in
  opt ir target
