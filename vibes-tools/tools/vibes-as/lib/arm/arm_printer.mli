(** Pretty-prints the IR program in ARM assembly format. *)
val ir : is_thumb:bool -> Types.Assembly.printer
