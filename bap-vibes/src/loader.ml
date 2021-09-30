(***********************************************************************
*
*   This module provides a custom "lightweight" loader for binaries,
*   configured by user-input.
*
*   An almost verbatim copy of the "raw" loader for BAP: this allows us
*   fine-grained control of the loading process, effectively allowing
*   us to disassemble only small chunks of a large binary.
*
*
**********************************************************************)


let _doc = {|
# DESCRIPTION

Provides a VIBES specific loader for raw binaries. Raw binaries do not contain any
meta information or other headers, so this input should be provided
form the outside.

|}

open Bap.Std
open Core_kernel


module Buffer = Caml.Buffer
module Unix = Caml_unix

let doc_template = {|
(declare arch (name str))
(declare bits (size int))
(declare base-address (addr int))
(declare entry-point (addr int))
(declare is-little-endian (flag bool))
(declare mapped (addr int) (size int) (off int))
(declare code-region (addr int) (size int) (off int))
(declare named-region (addr int) (size int) (name str))
(declare segment (addr int) (size int) (r bool) (w bool) (x bool))
(declare section (addr int) (size int))
(declare code-start (addr int))
(declare named-symbol (addr int) (name str))

(arch $arch)
(bits $bits)
(base-address $base)
(entry-point $entry)
(is-little-endian $endian)
(mapped $base $length $offset)
(code-region $base $length $offset)
(named-region $base $length code)
(section $base $length)
(segment $base $length true false true)
(named-symbol $entry $func_name)
|}

(* FIXME: add this to the loader data? *)
let get_bits _ = 32

let get_func conf = Config.exe conf


let register_loader conf =
  match Config.loader_data conf with
  | None -> false
  | Some l_data ->
    begin
      Image.register_loader ~name:"vibes-raw" (module struct
        let generate measure input =
          let arch = Config.arch l_data |>
                     Option.value_map ~default:"" ~f:Arch.to_string
          in
          let options = [
            "arch", arch;
            "offset", Bitvec.to_string @@ Config.offset l_data;
            "base", Bitvec.to_string @@ Config.base l_data;
            "bits", Int.to_string @@ begin
              match Config.arch l_data with
              | None -> get_bits conf
              | Some arch -> Size.in_bits (Arch.addr_size arch)
            end;
            (* FIXME: probably we should handle more than one value? *)
            "entry", begin match Config.entry l_data with
              | [] -> Bitvec.to_string @@ Config.base l_data;
              | x :: _ -> Bitvec.to_string x
            end;
            "endian", Bool.to_string begin match Config.arch l_data with
            | None -> true
            | Some arch -> Poly.equal (Arch.endian arch) LittleEndian
            end;
            "length", Int64.to_string @@ begin match Config.length l_data with
              | None -> Int64.(measure input - Bitvec.(to_int64 @@ Config.offset l_data))
              | Some n -> n
            end;
            "func_name", get_func conf;
          ] |> String.Map.of_alist_exn in
          let buf = Buffer.create 128 in
          let ppf = Format.formatter_of_buffer buf in
          doc_template |>
          Buffer.add_substitute buf (fun var ->
              match Map.find options var with
              | None -> invalid_argf "bug: missed a var: %S" var ()
              | Some v -> v);
          Config.entry l_data |>
          List.iter ~f:(Format.fprintf ppf "(code-start %a)@\n" Bitvec.pp);
          Format.pp_print_flush ppf ();
          Buffer.contents buf |>
          Ogre.Doc.from_string |>
          Or_error.ok_exn

        let length_of_file filename =
          let desc = Unix.openfile filename Unix.[O_RDONLY] 0o400 in
          let {Unix.LargeFile.st_size; _} = Unix.LargeFile.fstat desc in
          Unix.close desc;
          st_size

        let length_of_data str =
          Int64.of_int (Bigstring.length str)

        let from_file name =
          Or_error.try_with @@ fun () ->
          Some (generate length_of_file name)

        let from_data data =
          Or_error.try_with @@ fun () ->
          Some (generate length_of_data data)
      end)
    end;
    true

