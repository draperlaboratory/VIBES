type Vibes_error_lib.Std.t +=
  | Other of string

let printer (e : Vibes_error_lib.Std.t) : string option =
  match e with
  | Other s -> Some s
  | _ -> None

let () = Vibes_error_lib.Std.register_printer printer
