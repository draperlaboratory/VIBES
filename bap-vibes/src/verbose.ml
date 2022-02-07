(***************************************************************************)
(*                                                                         *)
(*  Copyright (C) 2022/2023 The Charles Stark Draper Laboratory, Inc.      *)
(*                                                                         *)
(*  This file is provided under the license found in the LICENSE file in   *)
(*  the top-level directory of this project.                               *)
(*                                                                         *)
(*  This research was developed with funding from the Defense Advanced     *)
(*  Research Projects Agency (DARPA).                                      *)
(*                                                                         *)
(***************************************************************************)

(* Implements {!Verbose}. *)

open !Core_kernel

(* For configuring a verbose log. *)
module type Config = sig
  val with_colors : bool
end

(* The verbose log writes to stderr. *)
module Stderr (Config : Config) = struct

  let with_colors = Config.with_colors
  let frmttr = Format.err_formatter

  let indent = 4
  let header_char = '='
  let rule_char = '-'
  let header_width = 72
  let rule_width = 32

  let pad (msg : string) : string =
    let diff = header_width - (indent + (String.length msg)) in
    match diff <= 0 with
    | true -> ""
    | false -> String.make diff header_char

  let pp_header ppf msg =
    Format.pp_open_box ppf 0;
    Format.pp_force_newline ppf ();
    Format.pp_print_string ppf (String.make (indent - 1) header_char);
    Format.pp_print_string ppf " ";
    let () = match with_colors with
      | true -> Format.pp_print_string ppf "\x1b[1;33m"
      | false -> ()
    in
    Format.pp_print_string ppf msg;
    let () = match with_colors with
      | true -> Format.pp_print_string ppf "\x1b[0m"
      | false -> ()
    in
    Format.pp_print_string ppf " ";
    Format.pp_print_string ppf (pad msg);
    Format.pp_close_box ppf ();
    Format.pp_print_newline ppf ()

  let pp_info_line ppf msg =
    Format.pp_open_hbox ppf ();
    Format.pp_print_string ppf msg;
    Format.pp_close_box ppf ()

  let pp_info ppf msg =
    Format.pp_open_vbox ppf indent;
    Format.pp_print_string ppf (String.make indent ' ');
    Format.pp_print_list pp_info_line ppf (String.split_lines msg);
    Format.pp_close_box ppf ();
    Format.pp_print_newline ppf ()

  let pp_rule ppf () =
    Format.pp_open_hbox ppf ();
    Format.pp_print_string ppf (String.make indent ' ');
    Format.pp_print_string ppf (String.make rule_width rule_char);
    Format.pp_close_box ppf ();
    Format.pp_print_newline ppf ()

  let header s = Format.fprintf frmttr "%a" pp_header s
  let info s = Format.fprintf frmttr "%a" pp_info s
  let rule () = Format.fprintf frmttr "%a" pp_rule ()

  let handle event =
    match event with
    | Events.Header msg -> header msg
    | Events.Info msg -> info msg
    | Events.Rule -> rule ()

end
