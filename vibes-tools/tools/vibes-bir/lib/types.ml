type Vibes_error_lib.Std.t +=
  | No_blks of string

let printer (e : Vibes_error_lib.Std.t) : string option =
  match e with
  | No_blks s -> Some s
  | _ -> None

let () = Vibes_error_lib.Std.register_printer printer
