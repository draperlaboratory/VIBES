type Vibes_error_lib.Std.t +=
  | Higher_var_not_substituted of string

let printer (e : Vibes_error_lib.Std.t) : string option =
  match e with
  | Higher_var_not_substituted s -> Some s
  | _ -> None

let () = Vibes_error_lib.Std.register_printer printer
