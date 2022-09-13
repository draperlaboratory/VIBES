open Core
open Bap.Std
open Bap_core_theory

let name = "vibes-ogre"
let registered = ref false

let register (data : string) : unit =
  if not !registered then begin
    Image.register_loader ~name (module struct
      let from_file _ = Or_error.map ~f:Option.some (Ogre.Doc.from_string data)
      let from_data _ = Or_error.map ~f:Option.some (Ogre.Doc.from_string data)
    end);
    registered := true
  end

let image
    ?(backend : string = "llvm")
    (filepath : string) : (image, KB.conflict) result =
  match Image.create filepath ~backend with
  | Ok (image, _) -> Ok image
  | Error err ->
    let msg = Format.asprintf "Error loading %s: %a" filepath Error.pp err in
    Error (Errors.Invalid_binary msg)
