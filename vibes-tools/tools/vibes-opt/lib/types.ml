type Vibes_error_lib.Std.t +=
  | No_bir of string
  | Invalid_bir of string
  | Unknown_target of string

let printer (e : Vibes_error_lib.Std.t) : string option =
  match e with
  | No_bir s -> Some s
  | Invalid_bir s -> Some s
  | Unknown_target s -> Some s
  | _ -> None

let () = Vibes_error_lib.Std.register_printer printer
