open !Core_kernel
open Bap.Std
open Bap_knowledge
open Bap_vibes
open OUnit2

module KB = Knowledge


(* Create an empty project. *)
let empty_proj (filename : string) : (Project.t, Error.t) result =
  let arch = `x86_64 in
  let code = Memmap.empty in
  let data = Memmap.empty in
  let input = Project.Input.create arch filename ~code ~data in
  Project.create input

(* Same as [empty_proj], but fail if loading errors. *)
let proj_exn ( proj : (Project.t, Error.t) result) : Project.t =
  match proj with
  | Ok p -> p
  | Error e ->
    begin
      let msg = Printf.sprintf "Load error: %s" (Error.to_string_hum e) in
      failwith msg
    end

(* Get an empty program that can be used in tests. *)
let prog_exn (proj : (Project.t, Error.t) result) : Program.t =
  let p = proj_exn proj in
  Project.program p

(* Some dummy values that can be used in tests. *)
let patch = "ret-3"
let patch_point_str = "0x3f"
let patch_point = Bitvec.of_string patch_point_str
let patch_size = 16
let property_str = "true"
let property = Sexp.Atom property_str
let assembly = ["%00000001:"; "mov R0, #3"; "%00000002:"]
let original_exe = "/path/to/original/exe"
let patched_exe = "/path/to/patched/exe"
let proj = empty_proj original_exe
let prog = prog_exn proj

(* A BAP loader for testing. No disk I/O. Just wraps [proj] *)
let loader (_ : string) : Project.t KB.t =
  match proj with
  | Ok p -> KB.return p
  | Error e ->
    begin
      let msg = Printf.sprintf "Load error: %s" (Error.to_string_hum e) in
      Errors.fail (Errors.Failed_to_load_proj msg)
    end


(* A helper to create a [Data] object that can be used in tests. *)
let obj () = KB.Object.create Data.cls

(* After a [kb_run] computation, assert that a property of the returned [obj]
   has a particular value. [property] is the property you want to check,
   [expected] is the value you expect it to have, and [result] is the result
   of a [kb_run]. [~cmp] takes a comparison function, and [~printer] takes
   a custom printer for the value (it should return a string version of the
   value). *)
let assert_property ~cmp ?p_res ?p_expected
    property expected result : unit =
  match result with
  | Ok (obj, _) ->
    begin
      let actual = KB.Value.get property obj in
      let msg =
        match p_res, p_expected with
        | Some p, Some p' ->
          begin
            Format.asprintf "@[<v 4>%s:@,@[%s@]@]@.@[<v 4>%s:@,@[%s@]@]"
              "expected" (p' expected) "but got" (p actual)
          end
        | _ ->
          Format.sprintf "Property did not have the expected value"
      in
      assert_bool msg (cmp expected actual)
    end
  | Error problem ->
    let msg = Format.asprintf
        "@[<v 4>expected a value, but got an error:@,@[%a@]@]"
        KB.Conflict.pp problem
    in
    assert_bool msg false

(* After a [kb_run] computation, assert that the comutation diverged with
   a particular error. [property] is the property you want to check,
   [expected] is the error you expect, and [result] is the result of
   [kb_run]. [~printer] takes a custom printer for the value,
   which should return a string version of the value. *)
let assert_error ?printer property expected result : unit =
  match result with
  | Ok (obj, _) ->
    begin
      let actual = KB.Value.get property obj in
      let msg =
        match printer with
        | Some p ->
          begin
            Format.asprintf "@[<v 4>%s:@,@[%s@]@]@.@[<v 4>%s:@,@[%s@]@]"
              "expected error"
              (Format.asprintf "%a" KB.Conflict.pp expected)
              "but got value"
              (p actual)
          end
        | None ->
          begin
            Format.asprintf
              "@[<v 4>Got a value, but expected error:@,@[%a@]@]"
              KB.Conflict.pp expected
          end
      in
      assert_bool msg false
    end
  | Error problem ->
    begin
      let msg = Format.asprintf "@[<v 4>%s:@,@[%s@]@]@.@[<v 4>%s:@,@[%s@]@]"
          "expected error"
          (Format.asprintf "%a" KB.Conflict.pp expected)
          "but got this error"
          (Format.asprintf "%a" KB.Conflict.pp problem)
      in
      assert_bool msg String.((KB.Conflict.to_string problem) = (KB.Conflict.to_string expected))
    end

(* A printer for optional string values, to be used as a printer
   for the [assert_property] and [assert_error] functions above. *)
let print_opt opt =
  match opt with
  | Some x -> Printf.sprintf "Some %s" x
  | None -> "None"

(* Similar to [print_opt], but for optional ints. *)
let print_int_opt opt =
  match opt with
  | Some x -> Printf.sprintf "Some %d" x
  | None -> "None"

(* Similar to [print_opt], but for optional string lists. *)
let print_string_list_opt items =
  match items with
  | Some xs -> Printf.sprintf "Some [%s]" (String.concat ~sep:"\n" xs)
  | None -> "None"

(* Pretty print programs. *)
let print_prog prog = Format.asprintf "%a" Program.pp prog
let print_prog_opt opt =
  match opt with
  | Some prog -> print_prog prog
  | None -> "None"

(* Pretty print BIL. *)
let print_bil bil = Format.asprintf "%a" Bil.pp bil

(* A verifier function for testing. It always returns unsat. *)
let verify_unsat (_ : Program.t) (_ : Program.t) (_ : string) (_ : Sexp.t)
  : Z3.Solver.status =
  Z3.Solver.UNSATISFIABLE

(* A verifier function for testing. It always returns sat. *)
let verify_sat (_ : Program.t) (_ : Program.t) (_ : string) (_ : Sexp.t)
  : Z3.Solver.status =
  Z3.Solver.SATISFIABLE
