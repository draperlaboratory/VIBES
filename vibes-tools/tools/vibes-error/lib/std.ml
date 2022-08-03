open Core

type t = ..
type t += Failure of string

type printer = t -> string option

let printers : (int, printer) Hashtbl.t = Hashtbl.create (module Int)

let register_printer (printer : printer) : unit =
  let next_slot = Hashtbl.length printers in
  Hashtbl.set printers ~key:next_slot ~data:printer

let local_printer t : string option =
  match t with
  | Failure s -> Some s
  | _ -> None

let () = register_printer local_printer

let to_string t =
  let result =
    Hashtbl.fold printers ~init:"" ~f:(fun ~key:_ ~data:printer acc ->
      match printer t with
      | Some s -> s
      | None -> acc)
  in
  if String.is_empty result then "Unprintable error (no printer registered)"
  else result
