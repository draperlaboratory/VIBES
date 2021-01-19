(* Implements {!Patcher}. *)

open Bap_knowledge
open Knowledge.Syntax

module KB = Knowledge
module In = Core_kernel.In_channel
module Out = Core_kernel.Out_channel

let run_process (command : string) (args : string list) : (unit, Errors.t) Result.t =
  let (as_stdout, as_stdin) =
    Unix.open_process_args command (Array.of_list (command :: args)) in
  let status = Unix.close_process (as_stdout, as_stdin) in
  match status with
  | WEXITED 0 -> Ok ()
  | WEXITED 127 ->
    begin
      let msg = Format.sprintf "'%s' not found in PATH" command in
      Error (Errors.Command_not_found msg)
    end
  | WEXITED n ->
    begin
      let msg = Format.sprintf "%s returned exit code: %d" command n in
      Error (Errors.Exit_code msg)
    end
  | _ ->
    begin
      let msg =
        Format.sprintf "%s exited with unknown return status" command in
      Error (Errors.Unexpected_exit msg)
    end

(** [binary_of_asm] uses external programs to convert assembly code to binary *)
let binary_of_asm (assembly : string list) : (string, Errors.t) Result.t = 
  (* Write assembly to temporary file *)
  let asm_filename = Stdlib.Filename.temp_file "vibes-assembly" ".asm" in
  Out.write_lines asm_filename assembly;

  (* run assembler *)
  let assembler = "/usr/bin/arm-linux-gnueabi-as" in
  let with_elf_filename = Stdlib.Filename.temp_file "vibes-assembly" ".o" in
  Result.bind (run_process assembler ["-o"; with_elf_filename ; asm_filename ]) (fun _ -> 
      (* strip elf data *)
      let objcopy = "/usr/bin/arm-linux-gnueabi-objcopy" in
      let raw_bin_filename = Stdlib.Filename.temp_file "vibes-assembly" ".bin" in
      let objcopy_args = ["-O"; "binary"; with_elf_filename; raw_bin_filename ] in
      Result.bind (run_process objcopy objcopy_args) (fun _ ->
          let patch_exe = In.read_all raw_bin_filename in
          Ok patch_exe
        ))

(* [relative_jmp] produces binary for an unconditional relative jump *)
let relative_jmp (rel_jmp : int) : (string, Errors.t) Result.t = 
  let branch_relative = 
    [  "_start:";
       Printf.sprintf "b (_start + (%d))" rel_jmp; (* relative direct jump in arm *)
       "" ] 
  in
  binary_of_asm branch_relative

(** [lift_kb] lifts the Result monad to the KB monad *)
let lift_kb (x : ('a, Errors.t) Result.t) : 'a KB.t =
  match x with
  | Ok x -> KB.return x
  | Error e -> Errors.fail e

let patch_file (original_exe_filename : string) (patches : (int64 * string) list) : string =
  let tmp_patched_exe_filename =
    Stdlib.Filename.temp_file "vibes-assembly" ".patched" in
  let orig_exe = In.read_all original_exe_filename in
  Out.with_file tmp_patched_exe_filename ~f:(fun file ->
      Out.output_string file orig_exe;
      Core_kernel.List.iter patches ~f:(
        fun (loc, patch) -> 
          Out.seek file loc;
          Out.output_string file patch
      ));
  tmp_patched_exe_filename


(* TODO: Surely there must be a better way *)
let find_dummy_region (filename : string) : int =
  let command = Printf.sprintf  "objdump %s -dF | grep vibes_dummy | grep -oP \"(?<=File Offset: 0x)([0-9a-fA-F]+)\"" filename in
  let in_channel = Unix.open_process_in command in
  let addr_string = input_line in_channel in 
  Scanf.sscanf addr_string "%x" (fun i -> i)

let dummy_addr_ref : (int option) ref = ref None



(* This function performs the actual patching of a binary. *)
let patch_naive (original_exe_filename : string) (assembly : string list)
    (patch_point : Bitvec.t) (target_size : int): string KB.t =
  let (let*) x f = Result.bind x f in
  let patch_point = Bitvec.to_int64 patch_point in
  
  (* Initially let's assume the patch placement is the patch point *)
  let patch_relative (pr : int) : string = Printf.sprintf ".equiv relative_patch_placement, %d\n" pr in
  let test_assembly = (patch_relative 0) :: assembly in

  (* Read in original binary and patch  *)
  lift_kb (binary_of_asm test_assembly) >>= (fun test_patch_exe -> 
      let patch_size = String.length test_patch_exe in
      let patches : ((int64 * string) list, Errors.t) Result.t = 
        if (patch_size > target_size) (* If patch doesn't fit *)
        then  begin
          (* look up dummy address or compute it *)
          let dummy_addr = match !dummy_addr_ref with
            | None -> find_dummy_region original_exe_filename
            | Some a -> a
          in
          (* Increment dummy_address for next time to avoid patch overlap *)
          dummy_addr_ref := Some (dummy_addr + patch_size + 4);
          
          (* Calculate patch placement relative to patch point and recompile patch *)
          let patch_relative_location = dummy_addr - (Int64.to_int patch_point) in
          let assembly = (patch_relative patch_relative_location) :: assembly in
          let* patch_exe = binary_of_asm assembly in

          let jmp_val = (Int64.to_int patch_point) + target_size - (dummy_addr + patch_size) in
          let* rel_jmp_exe = relative_jmp jmp_val in
          
          let patch_exe = patch_exe ^ rel_jmp_exe in
          let* jmp_to_patch = relative_jmp (dummy_addr - (Int64.to_int patch_point)) in
          
          Ok [ (Int64.of_int dummy_addr , patch_exe ) ;  (patch_point, jmp_to_patch) ]
        end
        else Ok [ patch_point , test_patch_exe ] 
      in
      let patches = Result.get_ok patches in
      KB.return (patch_file original_exe_filename patches))


(* Applies a single patch to an exe, producing an update exe. *)
let patch_one (patcher : string -> string list -> Bitvec.t -> int -> string KB.t)
    (count_name : (int*string) KB.t) (patch : Data.Patch.t)
  : (int*string) KB.t =
  count_name >>= fun (n,orig) ->
  Events.(send @@ Info (Printf.sprintf "\nApplying patch %d to %s..."
                          n orig));
  Data.Patch.get_patch_point_exn patch >>= fun patch_point ->
  Data.Patch.get_patch_size_exn patch >>= fun patch_size ->
  Data.Patch.get_assembly_exn patch >>= fun assembly ->
  patcher orig assembly patch_point patch_size >>= fun patched_loc ->
  Events.(send @@ Info
            (Printf.sprintf "Patch %d applied. Temporary patched exe filepath: %s"
               n patched_loc));
  KB.return (n+1,patched_loc)

(* Patches the original exe, to produce a patched exe. *)
let patch ?patcher:(patcher=patch_naive) (obj : Data.t) : unit KB.t =
  Events.(send @@ Header "Starting patcher");

  (* Get patch information (the address to start patching and the number
     of bytes to overwrite), and get the patch assembly. *)
  Events.(send @@ Info "Retrieving data from KB...");
  dummy_addr_ref := None;
  Data.Original_exe.get_filepath_exn obj >>= fun original_exe_filename ->
  Data.Patched_exe.get_patches obj >>= fun patches ->
  Data.Patch_set.fold patches
    ~init:(KB.return (1,original_exe_filename))
    ~f:(patch_one patcher)>>= fun (_,tmp_patched_exe_filename) ->

  (* Stash the filepath in the KB. *)
  Data.Patched_exe.set_tmp_filepath obj
    (Some tmp_patched_exe_filename) >>= fun _ ->

  KB.return ()
