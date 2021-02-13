(* Implements {!Vibes_plugin_utils}. *)

(* [input_lines ic] reads all lines from [ic] and returns the string list. *)
let rec input_lines ?acc:(acc = []) (ic : in_channel) : string list =
    match input_line ic with
    | line -> input_lines ic ~acc:(line :: acc)
    | exception End_of_file ->
      close_in ic;
      List.rev acc

(* [shell cmd] runs [cmd] in the shell, returns output as a string list. *)
let shell (cmd : string) : string list =
  let ic = Unix.open_process_in cmd in
  let result = input_lines ic in
  result

(* [realpath "~/foo/bar"] returns the real path of [~/foo/bar]. *)
let realpath (path : string) : string =
  let cmd = "realpath " ^ path in
  let shell_output = shell cmd in
  match shell_output with
  | [] -> path
  | expanded_path::[] -> expanded_path
  | head::tail -> path
