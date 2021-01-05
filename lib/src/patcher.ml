(* Implements {!Patcher}. *)

open Bap_knowledge
open Knowledge.Syntax

module KB = Knowledge
module In = Core_kernel.In_channel
module Out = Core_kernel.Out_channel

let run_process_exn (command : string) (args : string list) : unit KB.t =
  let (as_stdout, as_stdin) =
    Unix.open_process_args command (Array.of_list (command :: args)) in
  let status = Unix.close_process (as_stdout, as_stdin) in
  match status with
  | WEXITED 0 -> KB.return ()
  | WEXITED 127 ->
    begin
      let msg = Format.sprintf "'%s' not found in PATH" command in
      Errors.fail (Errors.Command_not_found msg)
    end
  | WEXITED n ->
    begin
      let msg = Format.sprintf "%s returned exit code: %d" command n in
      Errors.fail (Errors.Exit_code msg)
    end
  | _ ->
    begin
      let msg =
        Format.sprintf "%s exited with unknown return status" command in
      Errors.fail (Errors.Unexpected_exit msg)
    end

(* This function performs the actual patching of a binary. *)
let patch_naive (original_exe_filename : string) (assembly : string list)
    (patch_point : Bitvec.t) : string KB.t =

  (* Write assembly to temporary file *)
  let asm_filename = Stdlib.Filename.temp_file "vibes-assembly" ".asm" in
  Out.write_lines asm_filename assembly;

  (* run assembler *)
  let assembler = "/usr/bin/arm-linux-gnueabi-as" in
  let with_elf_filename = Stdlib.Filename.temp_file "vibes-assembly" ".o" in
  run_process_exn
    assembler ["-o"; with_elf_filename ; asm_filename ] >>= fun () ->

  (* strip elf data *)
  let objcopy = "/usr/bin/arm-linux-gnueabi-objcopy" in
  let raw_bin_filename = Stdlib.Filename.temp_file "vibes-assembly" ".bin" in
  let objcopy_args = ["-O"; "binary"; with_elf_filename; raw_bin_filename ] in
  run_process_exn objcopy objcopy_args >>= fun () ->

  (* Read in original binary and patch  *)
  let tmp_patched_exe_filename =
    Stdlib.Filename.temp_file "vibes-assembly" ".patched" in
  let patch_exe = In.read_all raw_bin_filename in
  let orig_exe = In.read_all original_exe_filename in
  let patch_location = Bitvec.to_int64 patch_point in
  Out.with_file tmp_patched_exe_filename ~f:(fun file ->
      Out.output_string file orig_exe;
      Out.seek file patch_location;
      Out.output_string file patch_exe);

  KB.return tmp_patched_exe_filename

(* Patches the original exe, to produce a patched exe. *)
let patch ?patcher:(patcher=patch_naive) (obj : Data.t) : unit KB.t =
  Events.(send @@ Header "Starting patcher");

  (* Get patch information (the address to start patching and the number
     of bytes to overwrite), and get the patch assembly. *)
  Events.(send @@ Info "Retrieving data from KB...");
  Data.Original_exe.get_filepath_exn obj >>= fun original_exe_filename ->
  Data.Patched_exe.get_patch_point_exn obj >>= fun patch_point ->
  Data.Patch.get_assembly_exn obj >>= fun assembly ->

  (* Do the patch, using the specified patcher. *)
  patcher original_exe_filename
    assembly patch_point >>= fun tmp_patched_exe_filename ->

  (* Report the results and stash the filepath in the KB. *)
  Events.(send @@
          Info (Core_kernel.sprintf "Temporary patched exe filepath: %s"
                  tmp_patched_exe_filename));
  Data.Patched_exe.set_tmp_filepath obj
    (Some tmp_patched_exe_filename) >>= fun _ ->

  KB.return ()
