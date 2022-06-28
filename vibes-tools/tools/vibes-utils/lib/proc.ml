module Err = Vibes_error_lib.Std
module Log = Vibes_log_lib.Stream

type stdout_data = string list
type stderr_data = string list
type cmd_result = (stdout_data * stderr_data, Err.t) result

let run (command : string) (args : string list) : cmd_result =
  let env = Core_kernel.Array.init 0 ~f:(fun _ -> "") in
  let cmd = Core_kernel.String.concat (command :: args) ~sep:" " in
  Log.send (Format.sprintf "Running cmd: '%s'" cmd);
  let (std_out, std_in, std_err) = Unix.open_process_full cmd env in
  let output = Core_kernel.In_channel.input_lines std_out in
  Log.send (Format.sprintf "STDOUT:\n%s" (Core_kernel.String.concat output ~sep:"\n"));
  let error = Core_kernel.In_channel.input_lines std_err in
  Log.send (Format.sprintf "STDERR:\n%s" (Core_kernel.String.concat error ~sep:"\n"));
  Log.send ("Closing process channels");
  let status = Unix.close_process_full (std_out, std_in, std_err) in
  match status with
  | WEXITED 0 ->
    Log.send "Exit code: 0";
    Ok (output, error)
  | WEXITED 127 ->
    Log.send "Exit code: 127";
    let msg = Format.sprintf "'%s' not found on path\n%!" command in
    Error (Types.Not_on_path msg)
  | WEXITED n ->
    Log.send (Format.sprintf "Exit code: '%d'" n);
    let msg = Format.sprintf "Exited with non-zero exit code '%d'\n%!" n in
    Error (Types.Bad_exit_code msg)
  | _ ->
    Log.send "Unknown exit status";
    let out_lines = Core_kernel.String.concat output ~sep:"\n" in
    let err_lines = Core_kernel.String.concat error ~sep:"\n" in
    let msg =
      Format.sprintf 
        "Command exited with unknown status.\n \
         COMMAND:\n%s\n \
         STDOUT:\n%s\n \
         STDERR:\n%s\n"
        cmd out_lines err_lines
    in
    Error (Types.Unknown_exit msg)
