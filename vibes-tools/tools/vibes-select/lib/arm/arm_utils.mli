(** Returns the variable for a register name, depending on
    whether we're in Thumb mode or not. *)
val preassign : is_thumb:bool -> Types.Preassign.transform
