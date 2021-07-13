open !Core_kernel
open Bap.Std
open Bap_knowledge
open Bap_vibes
open Bap_core_theory
open OUnit2

module KB = Knowledge

(* Skip a test, with a message saying why. *)
let skip_test (msg : string) : unit = skip_if true msg

(* Create an empty project. *)
let empty_proj (filename : string) : (Project.t, Error.t) result =
  let arch = `armv7 in
  let code = Memmap.empty in
  let data = Memmap.empty in
  let input = Project.Input.create arch filename ~code ~data in
  Project.create input

(* Same as [empty_proj], but fail if loading errors. *)
let proj_exn (proj : (Project.t * string, Error.t) result) : Project.t * string =
  match proj with
  | Ok p -> p
  | Error e ->
    begin
      let msg = Printf.sprintf "Load error: %s" (Error.to_string_hum e) in
      failwith msg
    end

(* Create a dummy project with an empty main subroutine *)
let dummy_proj ?name:(name = "main") filename : (Project.t * string, Error.t) result =
  let empty_proj = empty_proj filename in
  let dummy_main = Sub.create ~name:name () in
  let dummy_prog = Program.Builder.create () in
  Program.Builder.add_sub dummy_prog dummy_main;
  let dummy_prog = Program.Builder.result dummy_prog in
  Result.map empty_proj ~f:(fun p -> (Project.with_program p dummy_prog, filename))

(* Get an empty program that can be used in tests. *)
let prog_exn (proj : (Project.t * string, Error.t) result) : Program.t * string =
  let p, s = proj_exn proj in
  (Project.program p, s)

let dummy_target =
  Theory.Target.declare
    ~bits:32
    ~byte:8
    "dummy_tgt"

(* Some dummy values that can be used in tests. *)
let patch = "ret-3"
let patch_point_str = "0x3f"
let patch_point = Bitvec.of_string patch_point_str
let patch_size = 16
let property_str = "(assert (= true true))"
let property = Sexp.Atom property_str
let func = "main"
let assembly = ["patch:"; "mov R0, #3"]
let original_exe = "/path/to/original/exe"
let patched_exe = "/path/to/patched/exe"
let minizinc_model_filepath = "/path/to/model.mzn"
let unit = KB.Object.create Bap_core_theory.Theory.Unit.cls
let proj = dummy_proj original_exe
let prog = prog_exn proj

(* A helper to create a [Data] object that can be used in tests. *)
let obj () = KB.Object.create Data.cls

(* After a [KB.run] computation, extract a given property from the returned
   object for further processing.  Fail if the KB computation failed. *)
let extract_property (property : ('k, 'a) KB.slot)
      (result : (('k, 's) KB.cls KB.value * KB.state, KB.conflict) result)
    : 'a =
  match result with
  | Ok (value, _) -> KB.Value.get property value
  | Error problem ->
    let msg = Format.asprintf
        "@[<v 4>expected a value, but got an error:@,@[%a@]@]"
        KB.Conflict.pp problem
    in
    assert_failure msg

(* After a [kb_run] computation, assert that a property of the returned [obj]
   has a particular value. [property] is the property you want to check,
   [expected] is the value you expect it to have, and [result] is the result
   of a [kb_run]. [~cmp] takes a comparison function, and [~printer] takes
   a custom printer for the value (it should return a string version of the
   value). *)
let assert_property ~cmp ?p_res ?p_expected
    property expected result : unit =
  let actual = extract_property property result in
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
      assert_bool msg String.(
        (KB.Conflict.to_string problem) = (KB.Conflict.to_string expected)
      )
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

(* Print a list of strings, with newlines between elements *)
let print_string_list items =
  Printf.sprintf "[%s]" (String.concat ~sep:"\n" items)

(* Similar to [print_opt], but for optional string lists. *)
let print_string_list_opt items =
  match items with
  | Some xs -> Printf.sprintf "Some %s" (print_string_list xs)
  | None -> "None"

(* Pretty print programs. *)
let print_prog prog = Format.asprintf "%a" Bap.Std.Program.pp prog
let print_prog_opt opt =
  match opt with
  | Some prog -> print_prog prog
  | None -> "None"

(* Pretty print BIR. *)
let print_bir (bir : Insn.t) =
  Format.asprintf "%a" Insn.pp_adt bir

(* A verifier function for testing. It always returns unsat. *)
let verify_unsat
    (_ : Bap_wp.Run_parameters.t)
    (_ : Bap_wp.Runner.input list)
    : (Verifier.status, Bap_main.error) result =
  Result.return Z3.Solver.UNSATISFIABLE

(* A verifier function for testing. It always returns sat. *)
let verify_sat
    (_ : Bap_wp.Run_parameters.t)
    (_ : Bap_wp.Runner.input list)
    : (Verifier.status, Bap_main.error) result =
  Result.return Z3.Solver.SATISFIABLE

(* A verifier function for testing. It always returns unknown. *)
let verify_unknown
    (_ : Bap_wp.Run_parameters.t)
    (_ : Bap_wp.Runner.input list)
    : (Verifier.status, Bap_main.error) result =
  Result.return Z3.Solver.UNKNOWN

(* A verifier printer function for testing. It does nothing. *)
let verifier_printer (_ : (_, _) result) : unit = ()
