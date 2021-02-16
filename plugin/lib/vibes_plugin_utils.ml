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
  input_lines ic

(* [realpath path] returns the realpath of [path], or an error. *)
let realpath (path : string) : (string, Vibes_plugin_errors.t) result =
  let cmd = "realpath " ^ path in
  let shell_output = shell cmd in
  let path =
    if (List.length shell_output) = 1 then List.hd shell_output
    else path
  in
  if Sys.file_exists path then Ok path
  else Error (Vibes_plugin_errors.No_such_file path)
