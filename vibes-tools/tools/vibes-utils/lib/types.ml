type Vibes_error_lib.Std.t +=
   | Not_on_path of string
   | Bad_exit_code of string
   | Unknown_exit of string
   | Failed_file_read of string
   | Failed_file_write of string

let printer (e : Vibes_error_lib.Std.t) : string option =
  match e with
  | Not_on_path s -> Some s
  | Bad_exit_code s -> Some s
  | Unknown_exit s -> Some s
  | Failed_file_read s -> Some s
  | Failed_file_write s -> Some s
  | _ -> None

let () = Vibes_error_lib.Std.register_printer printer
