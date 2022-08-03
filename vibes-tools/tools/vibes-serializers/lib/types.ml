type Vibes_error_lib.Std.t +=
  | Unhandled_bir of string
  | Invalid_bir of string
  | Invalid_func_info of string

let printer (e : Vibes_error_lib.Std.t) : string option =
  match e with
  | Unhandled_bir s -> Some s
  | Invalid_bir s -> Some s
  | Invalid_func_info s -> Some s
  | _ -> None

let () = Vibes_error_lib.Std.register_printer printer
