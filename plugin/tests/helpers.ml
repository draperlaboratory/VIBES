(* Some helpers for running tests. *)

open OUnit2

(* Some constants used for running ARM programs. *)
let qemu_arm = "qemu-arm"
let qemu_ld_prefix_env = "QEMU_LD_PREFIX=/usr/arm-linux-gnueabi"
let resources_exes_dir = "../resources/exes"

(* [exe_dir "simple-compiled"] will return the "simple-compiled" directory. *)
let exe_dir (name : string) : string =
  String.concat "/" [ resources_exes_dir; name ]

(* [run_arm_exe ["foo"; "-c"; "bar"] ~dir ~exit_code ~ctxt] will [cd] to
   the specified [~dir] in resources/exe, execute [foo -c bar], and then 
   check that it returns the specified [~exit_code]. *)
let run_arm_exe ~ctxt:(ctxt: test_ctxt) ~exit_code:(exit_code: int)
    ~dir:(dir: string) (cmd : string list) : unit =
  assert_command 
    ~exit_code:(Unix.WEXITED exit_code)
    ~chdir:(exe_dir dir)
    ~env:[| qemu_ld_prefix_env |]
    ~ctxt
    qemu_arm cmd

(* [run_make ["test"] ~dir ~ctxt] will [cd] to the specified [~dir] in
   resources/exe, and then run [make test], checking for a 0 exit code. *)
let run_make ~ctxt:(ctxt: test_ctxt) ~dir:(dir: string)
    (make_args: string list) : unit =
  assert_command
    ~exit_code:(Unix.WEXITED 0)
    ~chdir:(exe_dir dir)
    ~ctxt
    "make" make_args
