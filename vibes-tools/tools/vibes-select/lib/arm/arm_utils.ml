open Bap.Std

module Pre = Types.Preassign

let preassign ~(is_thumb : bool) : Pre.transform =
  fun typ -> function
    | "FP" when is_thumb -> Arm_env.r7
    | "FP" -> Arm_env.r11
    | n -> Var.create n typ
