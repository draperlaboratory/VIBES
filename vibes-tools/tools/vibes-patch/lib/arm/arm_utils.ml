open Core
open Bap_core_theory

module CT = Vibes_utils.Core_theory
module Constants = Vibes_constants.Asm
module Ops = Vibes_select.Arm_ops
module Asm = Vibes_as.Types.Assembly
module Proc = Vibes_utils.Proc
module Filename = Stdlib.Filename
module Log = Vibes_log.Stream

let (let*) x f = Result.bind x ~f

let assembler : string = "/usr/bin/arm-linux-gnueabi-as"
let objcopy : string = "/usr/bin/arm-linux-gnueabi-objcopy"

let max_insn_length : int = 4

let trampoline (addr : int64) : Asm.block =
  let op = Ops.b () in
  let b = Format.sprintf "%s (%s + %Ld - %s)"
      op
      Constants.patch_start_label
      addr
      Constants.patch_location in
  let label = Format.sprintf "trampoline%Ld" addr in
  Asm.Fields_of_block.create ~label ~insns:[b]

let create_trampoline
    (addr : int64)
    (patch_point : int64)
    (patch_size : int64) : Asm.t =
  let block = trampoline addr in
  Asm.Fields.create ~patch_point ~patch_size
    ~directives:[".syntax unified"]
    ~blocks:[block]

let insert_trampoline (addr : int64) (asm : Asm.t) : Asm.t =
  let block = trampoline addr in
  Asm.{asm with blocks = asm.blocks @ [block]}

let insert_overwritten
    (addr : int64)
    (asm : Asm.t)
    (insns : string list) : Asm.t =
  let label = Format.sprintf "overwritten%Ld" addr in
  let block = Asm.Fields_of_block.create ~label ~insns in
  Asm.{asm with blocks = asm.blocks @ [block]}

let has_ldr_large_const : Asm.t -> bool =
  let re = Str.regexp "ldr R[0-9]+, =[0-9]+" in
  fun asm -> Asm.blocks asm |> List.exists ~f:(fun block ->
      Asm.insns block |> List.exists ~f:(fun insn ->
          Str.string_match re insn 0))

let has_inline_data (asm : Asm.t) : bool =
  has_ldr_large_const asm

let ends_in_jump (asm : Asm.t) : bool = match List.last asm.blocks with
  | None -> false
  | Some block -> match List.last block.insns with
    | None -> false
    | Some insn -> String.is_prefix insn ~prefix:"b "

let adjusted_org (loc : int64) : int64 option =
  match Int64.rem loc 4L with
  | 0L -> None
  | org -> Some org

let situate
    ?(org : int64 option = None)
    ?(jmp : int64 option = None)
    ?(overwritten : string list = [])
    (asm : Asm.t)
    ~(loc : int64)
    ~(to_addr : int64 -> int64) : Asm.t =
  let asm =
    let dir =
      Format.sprintf ".equiv %s, %Ld"
        Constants.patch_location @@ to_addr loc in
    Asm.{asm with directives = asm.directives @ [dir]} in
  let asm = match org with
    | None -> asm
    | Some org ->
      let dir = Format.sprintf ".org %Ld" org in
      Asm.{asm with directives = asm.directives @ [dir]} in
  let asm =
    let label = Constants.patch_start_label in
    let start = Asm.Fields_of_block.create ~label ~insns:[] in
    Asm.{asm with blocks = start :: asm.blocks} in
  let asm =
    if Int64.(asm.Asm.patch_size = 0L)
    && not (List.is_empty overwritten) then
      insert_overwritten (to_addr loc) asm overwritten
    else asm in
  match jmp with
  | Some jmp -> insert_trampoline jmp asm
  | None -> asm

(* Sometimes we will end up with conditional branches that target
   a concrete address that is out of range relative to where we
   situated the patch. This is a (hacky) step to relax such branches
   such that they target a local block containing an unconditional
   branch to such a target, since unconditional branches have a much
   wider range. *)
module Relax = struct

  let rec interleave_pairs = function
    | x :: y :: rest -> (x, y) :: interleave_pairs (y :: rest)
    | [] | [_] -> []

  (* If we're targeting a concrete address, then this will be the
     sole operand. *)
  let start_prefix : string =
    Format.sprintf "(%s" Constants.patch_start_label

  (* Check if the branch targets a local block. *)
  let is_local_b (op : string) (o : string) : bool =
    String.is_prefix op ~prefix:"b" &&
    String.is_prefix o ~prefix:"blk"

  (* Check if the branch is conditional and it targets a concrete
     address. The conditional branches have an opcode length of
     at least 3. *)
  let is_nonlocal_bcc (op : string) (o : string) : bool =
    String.is_prefix op ~prefix:"b" &&
    String.length op >= 3 &&
    String.is_prefix o ~prefix:start_prefix

  (* Same as above, but unconditional. *)
  let is_nonlocal_b (op : string) (o : string) : bool =
    String.(op = "b") && String.is_prefix o ~prefix:start_prefix

  (* Potential fallthrough blocks. *)
  let collect_afters (asm : Asm.t) : string String.Map.t =
    let open Asm in
    interleave_pairs asm.blocks |>
    List.fold ~init:String.Map.empty ~f:(fun m (x, y) ->
        Map.set m ~key:x.label ~data:y.label)

  (* Collect the blocks with unconditional branches. *)
  let collect_unconds (asm : Asm.t) : String.Set.t =
    List.fold asm.blocks ~init:String.Set.empty ~f:(fun init b ->
        List.fold b.insns ~init ~f:(fun s i ->
            match String.split i ~on:' ' with
            | op :: o :: _ when is_nonlocal_b op o -> Set.add s b.label
            | [op; o] when is_local_b op o -> Set.add s b.label
            | _ -> s))

  (* Attempt to relax the conditional branches. *)
  let go (asm : Asm.t) : Asm.t =
    let open Asm in
    let afters = collect_afters asm in
    let uncond = collect_unconds asm in
    let blocks = List.concat_map asm.blocks ~f:(fun b ->
        let redir = ref None in
        let fall = ref false in
        let rlabel = b.label ^ "_redir" in
        let flabel = b.label ^ "_fall" in
        (* Assume that the blocks are in a normal form where there
           are at most two branch instructions as terminators.
           More specifically, we have at most one conditional branch
           and at most one unconditional branch. *)
        let insns = List.concat_map b.insns ~f:(fun i ->
            match String.split i ~on:' ' with
            | op :: o :: rest when is_nonlocal_bcc op o ->
              (* Relax this conditional branch. *)
              redir := Some (String.concat ("b" :: o :: rest) ~sep:" ");
              (* See if this block has any unconditional branches. If so,
                 then we don't need to handle fallthroughs. *)
              if Set.mem uncond b.label then
                [String.concat [op; rlabel] ~sep:" "]
              else
                (* There is an implied fallthrough here. We need to
                   insert an unconditional branch to that location,
                   and perhaps also insert a block if it doesn't
                   exist. *)
                let r = String.concat [op; rlabel] ~sep:" " in
                let f = match Map.find afters b.label with
                  | Some l -> "b " ^ l
                  | None ->
                    let f = "b " ^ flabel in
                    fall := true;
                    f in
                [r; f]
            | _ -> [i]) in
        match !redir with
        | None -> [b]
        | Some i ->
          (* Insert the blocks. *)
          let b = {b with insns} in
          let br = {label = rlabel; insns = [i]} in
          if !fall then
            let bf = {label = flabel; insns = []} in
            [b; br; bf]
          else [b; br]) in
    {asm with blocks}

end

module Toolchain = struct

  let conditional_branch_out_of_range (err : string list) : bool =
    let substring = "conditional branch out of range" in
    List.exists err ~f:(String.is_substring ~substring)

  let with_thumb
      (args : string list)
      (language : Theory.language) : string list =
    if CT.is_thumb language then "-mthumb" :: args else args

  let assemble
      (asm : Asm.t)
      (language : Theory.language) : (string, KB.conflict) result =
    let asmfile = Filename.temp_file "vibes" ".asm" in
    let objfile = Filename.temp_file "vibes" ".o" in
    let data = Format.asprintf "%a" Asm.pp asm in
    Out_channel.write_all asmfile ~data;
    let args = with_thumb ["-o"; objfile; asmfile] language in
    let _, err, failed = Proc.run_with_error assembler args in
    let* () =
      if failed then
        if conditional_branch_out_of_range err then
          let asm = Relax.go asm in
          let data = Format.asprintf "%a" Asm.pp asm in
          Log.send "Relaxing branches:\n%s\n" data;
          Out_channel.write_all asmfile ~data;
          Proc.run assembler args |> Result.map ~f:ignore
        else
          let err = String.concat err ~sep:"\n" in
          let msg = Format.sprintf "Assembler failed:\n%s" err in
          Error (Errors.Invalid_asm msg)
      else Ok () in
    Ok objfile

  let to_binary (objfile : string) : (string, KB.conflict) result =
    let binfile = Filename.temp_file "vibes" ".bin" in    
    let args = ["-O"; "binary"; objfile; binfile] in
    let* _ = Proc.run objcopy args in
    Ok (In_channel.read_all binfile)

end
