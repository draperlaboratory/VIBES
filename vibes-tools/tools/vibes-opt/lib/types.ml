open Core
open Bap.Std

type Vibes_error_lib.Std.t +=
  | No_bir of string
  | Invalid_bir of string
  | Invalid_func_infos of string
  | No_blks of string
  | No_SP of string
  | Bad_hvar_at_exit of string
  | Stack_loc_already_used of string

let printer (e : Vibes_error_lib.Std.t) : string option =
  match e with
  | No_bir s -> Some s
  | Invalid_bir s -> Some s
  | Invalid_func_infos s -> Some s
  | No_blks s -> Some s
  | No_SP s -> Some s
  | Bad_hvar_at_exit s -> Some s
  | Stack_loc_already_used s -> Some s
  | _ -> None

let () = Vibes_error_lib.Std.register_printer printer

type t = {
  bir : blk term list;
  cfg : Graphs.Tid.t;
  exclude_regs : String.Set.t;
  argument_tids : Tid.Set.t;
}

let create ?(exclude_regs = String.Set.empty) ~cfg ~argument_tids bir =
  { bir; cfg; exclude_regs; argument_tids; }
