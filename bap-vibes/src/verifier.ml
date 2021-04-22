(* Implements {!Verifier}. *)

open !Core_kernel

type result = (string, Toplevel_error.t) Result.t

type verifier = orig_exe_filepath:string -> patched_exe_filepath:string ->
  property:Sexp.t -> string -> result

type printer = result -> unit

type next_step =
  | Done
  | Again

type wp_result =
  | UNSAT
  | SAT of string
  | UNKNOWN of string

let get_wp_result (r : (string, Toplevel_error.t) Result.t)
    : (wp_result, Toplevel_error.t) Result.t =
  match r with
  | Error msg -> Error msg
  | Ok output -> 
    begin
      let find pattern = Core_kernel.String.substr_index output ~pattern in
      match find "UNSAT" with
      | Some _ -> Ok UNSAT
      | None ->
        begin
          match find "UNKNOWN" with
          | Some _ -> Ok (UNKNOWN output)
          | None ->
            begin 
              match find "SAT" with
              | Some _ -> Ok (SAT output)
              | None ->
                begin
                  let msg =
                    Printf.sprintf "WP returned unexpected output:\n%s\n"
                    output 
                  in
                  Error (Toplevel_error.WP_unexpected_output msg)
                end
            end
        end
    end

let wp_verifier ~orig_exe_filepath:(orig_exe_filepath : string)
    ~patched_exe_filepath:(patched_exe_filepath : string)
    ~property:(property : Sexp.t) (func : string) 
    : (string, Toplevel_error.t) Result.t =
  let ext_solver = "--ext-solver-path=boolector" in
  let inline = "--inline=.*" in
  let postcond = Sexp.to_string property in
  let wp_cmd = Printf.sprintf
    "bap wp %s %s --func=%s %s %s --postcond='%s'"
    orig_exe_filepath patched_exe_filepath func ext_solver inline postcond
  in
  Utils.run_process_2 wp_cmd

let naive_printer (r : result) : unit =
  match r with
  | Error _ -> Events.(send @@ Info "We had an error with WP")
  | Ok output ->
    begin
      Events.(send @@ Info "WP finished");
      Events.(send @@ Info output)
    end

let verify ?verifier:(verifier=wp_verifier) ?printer:(printer=naive_printer)
    ~orig_exe_filepath:(orig_exe_filepath : string)
    ~patched_exe_filepath:(patched_exe_filepath : string)
    ~property:(property : Sexp.t) (func : string)
    : (next_step, Toplevel_error.t) Core_kernel.result =
  Events.(send @@ Header "Starting Verifier");
  Events.(send @@ Info "Beginning weakest-precondition analysis...");
  let r = verifier ~orig_exe_filepath ~patched_exe_filepath ~property func in
  printer r; 
  match get_wp_result r with
  | Ok UNSAT ->
    begin
      Events.(send @@
        Info "Weakest-precondition analysis returned: correct");
      Events.(send @@ Info "The patched binary is correct");
      Ok Done
    end
  | Ok (SAT _) ->
    begin
      Events.(send @@
        Info "Weakest-precondition analysis returned: incorrect");
      Events.(send @@ Info "The patched binary is not correct");
      Events.(send @@ Info "Trying again");
      Ok Again
    end
  | Ok (UNKNOWN _) ->
    begin
      let msg = "Weakest-precondition analysis returned: unknown" in
      Events.(send @@ Info msg);
      Events.(send @@ Info "Unable to determine correctness of patched exe");
      Error (Toplevel_error.WP_result_unknown msg)
    end
  | Error err -> 
    begin
      let msg = Format.asprintf
        "Weakest-precondition analysis errored:\n%a" Toplevel_error.pp err in
      Events.(send @@ Info msg);
      Error (Toplevel_error.WP_result_unknown msg)
    end
