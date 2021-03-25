(* Implements {!Toplevel_error}. *)

open !Core_kernel

(* Errors we want to raise explicitly that occur outside of a [KB.run]. *)
type t =
  | Failed_to_load_proj of string
  | WP_result_unknown of string
  | Max_tries of int
  | No_value_in_KB of string
  | KB_error of Kb_error.t
  | Other of string

(* A pretty-printer for these errors. *)
let pp ppf (e : t) =
  let msg = match e with
    | Failed_to_load_proj s -> s
    | WP_result_unknown s -> s
    | Max_tries n -> Format.sprintf "Tried %d times. Giving up" n
    | No_value_in_KB s -> s
    | KB_error e -> Format.asprintf "%a" Kb_error.pp e
    | Other s -> s
  in
  Format.fprintf ppf "@[%s@]@." msg
