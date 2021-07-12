(* Implements {!Patch_ingester}. *)

open Bap.Std
open Bap_knowledge
open Knowledge.Syntax
open Core_kernel
open Bap_core_theory

module KB = Knowledge
open KB.Let

module CoreParser (Core : Theory.Core) = struct
  open Theory
  open Core

  module M = Map.Make(String)

  type vibes

  type vvar  = vibes Bitv.t var
  type vpure = vibes Bitv.t pure

  type vsort = vibes Bitv.t Value.sort

  type parse_state = {nm : string;
                      word_t : vsort;
                      mem : (vibes,vibes) mem;
                      vars : vvar M.t}


  let named_err (patch_name : string) (err : string) : Kb_error.t =
    Patch_code_not_parsed ("Patch code for patch " ^ patch_name ^ " " ^ err)

  let parse_var_decls (nm : string) (word_t : vsort) (ds : Sexp.t)
        : vvar M.t KB.t =
    (* These are vars that the user need not declare explicitly, and are
       handled specially later in the pipeline.  *)
    let default_vars : (string * vvar) list = []
    in
    let* var_sexps : Sexp.t list =
      match ds with
      | Sexp.List ((Sexp.Atom "var-decls") :: vars) -> KB.return vars
      | Sexp.Atom "var-decls" -> KB.return []
      | _ -> Kb_error.fail (named_err nm "must begin with var-decls")
    in
    let get_var_folder (v : Sexp.t) (vs : string list KB.t) =
      let* vs = vs in
      match v with
      | Sexp.Atom s -> KB.return (s :: vs)
      | Sexp.List _ -> Kb_error.fail (named_err nm "has invalid var-decls spec")
    in
    let* var_strings : string list =
      List.fold_right ~f:get_var_folder ~init:(KB.return []) var_sexps
    in
    let var_assocs : (string * vvar) list =
      default_vars @
        List.map ~f:(fun s -> (s,Var.define word_t s)) var_strings
    in
    match M.of_alist var_assocs with
    | `Duplicate_key k ->
       Kb_error.fail (named_err nm
                      (  "has duplicated or reserved variable \""
                       ^ k ^ "\" in declaration list"))
    | `Ok m -> KB.return m

  let parse_var (st : parse_state) (v : string) : vvar KB.t =
    match M.find st.vars v with
    | None -> Kb_error.fail (named_err st.nm ("uses undeclared variable " ^ v))
    | Some v -> KB.return v

  (* This is probably less than ideal. *)
  let parse_int (st : parse_state) (s : string) : int KB.t =
     try KB.return (Scanf.sscanf s "0x%x" (fun x -> x))
     with _ ->
       (* try to parse as decimal *)
       try KB.return (Scanf.sscanf s "%d" (fun x -> x))
       with _ ->
         (* neither worked *)
         Kb_error.fail (named_err st.nm
           (  "contains invalid value " ^ s
            ^ " where an int literal was expected"))

  let rec parse_pure (st : parse_state) (p : Sexp.t) : vpure KB.t =
    match p with
    | Sexp.Atom s ->
       begin
         (* Check if it's a variable.  Otherwise interpret it as an int. *)
         match M.find st.vars s with
         | Some v -> KB.return (var v)
         | None ->
            let* i : int = parse_int st s in
            KB.return (int st.word_t Bitvec.M32.(!!i))
       end
    | Sexp.List [Sexp.Atom "load"; src] ->
       let* src = parse_pure st src in
       KB.return (load st.mem src)
    | Sexp.List (Sexp.Atom "load" :: _) ->
       Kb_error.fail (named_err st.nm
         "contains an invalid load (load takes exactly 1 argument)")
    | Sexp.List [Sexp.Atom "loadw"; (Sexp.Atom bits); src] ->
       let* src = parse_pure st src in
       let* i = parse_int st bits in
       (* This b0 hard-codes little-endianness. Our ARM examples are little
          endian, though technically ARM also has a big endian mode.  *)
       KB.return (loadw (Bitv.define i) b0 st.mem src)
    | Sexp.List (Sexp.Atom "loadw" :: _) ->
       Kb_error.fail (named_err st.nm
         (  "contains an invalid loadw (loadw takes exactly 2 "
          ^ "arguments, the first of which must be an integer literal)"))
    | Sexp.List [Sexp.Atom "-"; v1; v2] ->
       let* v1 = parse_pure st v1 in
       let* v2 = parse_pure st v2 in
       KB.return (sub v1 v2)
    | Sexp.List (Sexp.Atom "-" :: _) ->
       Kb_error.fail (named_err st.nm
         "contains an invalid - (- takes exactly 2 arguments)")
    | Sexp.List [Sexp.Atom ">>"; v1; v2] ->
       let* v1 = parse_pure st v1 in
       let* v2 = parse_pure st v2 in
       (* We implement signed shift by default *)
       KB.return (shiftr b1 v1 v2)
    | Sexp.List (Sexp.Atom ">>" :: _) ->
       Kb_error.fail (named_err st.nm
         "contains an invalid >> (>> takes exactly 2 arguments)")
    | Sexp.List [Sexp.Atom "+"; v1; v2] ->
       let* v1 = parse_pure st v1 in
       let* v2 = parse_pure st v2 in
       KB.return (add v1 v2)
    | Sexp.List (Sexp.Atom "+" :: _) ->
       Kb_error.fail (named_err st.nm
         "contains an invalid + (+ takes exactly 2 arguments)")
    | Sexp.List [Sexp.Atom "*"; v1; v2] ->
       let* v1 = parse_pure st v1 in
       let* v2 = parse_pure st v2 in
       KB.return (mul v1 v2)
    | Sexp.List (Sexp.Atom "*" :: _) ->
       Kb_error.fail (named_err st.nm
         "contains an invalid * (* takes exactly 2 arguments)")
    | Sexp.List [Sexp.Atom "/"; v1; v2] ->
       let* v1 = parse_pure st v1 in
       let* v2 = parse_pure st v2 in
       KB.return (sdiv v1 v2)
    | Sexp.List (Sexp.Atom "/" :: _) ->
       Kb_error.fail (named_err st.nm
         "contains an invalid / (/ takes exactly 2 arguments)")
    | _ ->
       Kb_error.fail (named_err st.nm
                      ("contains invalid value " ^ Sexp.to_string p))

  let parse_stmt (st : parse_state) (sexps : Sexp.t) : data eff KB.t =
    match sexps with
    | Sexp.Atom s ->
       Kb_error.fail (named_err st.nm
                      ("uses unimplemented atomic statement " ^ s))
    | Sexp.List [] -> Kb_error.fail (named_err st.nm "contains empty statement")
    | Sexp.List [Sexp.Atom "set"; Sexp.Atom dest; value] ->
       let* (dest : vvar) = parse_var st dest in
       let* (value : vpure) = parse_pure st value in
       KB.return (set dest value)
    | Sexp.List (Sexp.Atom "set" :: _) ->
       Kb_error.fail (named_err st.nm
          ("contains a \"set\" not followed by a destination and value"))
    | Sexp.List (Sexp.Atom s :: _) ->
       Kb_error.fail (named_err st.nm ("uses unimplemented statement " ^ s))
    | Sexp.List (Sexp.List _ :: _) ->
       Kb_error.fail (named_err st.nm "has an invalid statement (list at head)")

  let parse_bool (st : parse_state) (b : Sexp.t) : Bool.t pure KB.t =
    match b with
    | Sexp.List [Sexp.Atom "=="; v1; v2] ->
       let* v1 = parse_pure st v1 in
       let* v2 = parse_pure st v2 in
       KB.return (eq v1 v2)
    | _ ->
       Kb_error.fail (named_err st.nm
                      ("contains invalid boolean value: " ^ Sexp.to_string b))

  let rec parse_ctrl (st : parse_state) (c : Sexp.t) : ctrl eff KB.t =
    match c with
    | Sexp.Atom "fallthrough" -> KB.return (perform Effect.Sort.fall)
    | Sexp.List [Sexp.Atom "branch"; cond; b1; b2] ->
       let* cond = parse_bool st cond in
       let* b1 = parse_ctrl st b1 in
       let* b2 = parse_ctrl st b2 in
       KB.return (branch cond b1 b2)
    | Sexp.List (Sexp.Atom "branch" :: _) ->
       Kb_error.fail (named_err st.nm
                      ("contains a branch without exactly three arguments: "
                       ^ Sexp.to_string c))
    | Sexp.List [Sexp.Atom "goto"; Sexp.Atom label] ->
       KB.return (goto (Tid.for_name label))
    | Sexp.List (Sexp.Atom "goto" :: _) ->
       Kb_error.fail (named_err st.nm
                      ("contains a goto without exactly one atomic argument: "
                       ^ Sexp.to_string c))
    | Sexp.List [Sexp.Atom "jmp"; dest] ->
       let* dest = parse_pure st dest in
       KB.return (jmp dest)
    | Sexp.List (Sexp.Atom "jmp" :: _) ->
       Kb_error.fail (named_err st.nm
                      ("contains a jmp without exactly one atomic argument: "
                       ^ Sexp.to_string c))
    | _ ->
       Kb_error.fail (named_err st.nm
                      ("ends with invalid control flow: " ^ Sexp.to_string c))

  let parse_bir (nm : string) (bits : int) (sexps : Sexp.t list) : insn KB.t =
    match sexps with
    | [] ->
       Kb_error.fail (named_err nm "must be a non-empty top-level list")
    | decls :: sexps ->
       let word_t : vsort = Bitv.define bits in
       (* not sure about this mem stuff- copying patches.ml *)
       let mem_t = Mem.define word_t word_t in
       let mem : (vibes,vibes) mem = var (Var.define mem_t "mem") in
       (* First thing in sexp must be variable declarations *)
       let* vars : vvar M.t = parse_var_decls nm word_t decls in
       let st : parse_state = {nm; word_t; mem; vars} in
       let* (stmt_sexps, ctrl_sexp) =
         match List.split_n sexps (List.length sexps - 1) with
         | (sss, [cs]) -> KB.return (sss,cs)
         | _ -> Kb_error.fail (named_err nm "has no body")
       in
       let* data : data eff list =
         KB.all (List.map ~f:(parse_stmt st) stmt_sexps) in
       let data_blk =
         List.fold_right ~init:(perform Effect.Sort.bot) ~f:seq data
       in
       let* ctrl_blk = parse_ctrl st ctrl_sexp in
       blk Theory.Label.null data_blk ctrl_blk

end

let provide_bir (tgt : Theory.target) (patch : Data.Patch.t) : unit KB.t =
  Theory.instance () >>=
  Theory.require >>= fun (module Core) ->
  let module CParser = Parse_c.Eval(Core) in
  Data.Patch.init_sem patch >>= fun () ->
  Data.Patch.get_patch_name_exn patch >>= fun name ->
  Data.Patch.get_patch_code_exn patch >>= fun code ->
  Events.(send @@ Info (Printf.sprintf "Patch named %s" name));

  (* Get the patch (as BIR). *)
  let* bir = CParser.c_patch_to_eff tgt code in

  Events.(send @@ Info "The patch has the following BIL:");
  Events.(send @@ Rule);
  let bir_str = Format.asprintf "%a" Bil.pp (KB.Value.get Bil.slot bir) in
  Events.(send @@ Info bir_str);
  Events.(send @@ Rule);
  Data.Patch.set_bir patch bir

(* Ingests a single patch, populating the relevant fields of the KB,
   most notably the semantics field of the corresponding patch. (and
   increments the [patch_num] counter). *)
let ingest_one (tgt : Theory.target) (patch_num : int KB.t) (patch : Data.Patch.t)
    : int KB.t =
  patch_num >>= fun patch_num ->
  Events.(send @@ Info (Printf.sprintf "\nIngesting patch %d." patch_num));
  provide_bir tgt patch >>= fun () ->
  KB.return @@ patch_num+1

(* Processes the whole patch associated with [obj], populating all the
   relevant KB slots with semantic data associated with the patch
   syntax. *)
let ingest (obj : Data.t) : unit KB.t =
  Events.(send @@ Header "Starting patch ingester");
  Events.(send @@ Info "Using hand-written BIL patches");

  Events.(send @@ Info "Retreiving data from KB...");
  Data.Original_exe.get_target_exn obj >>= fun tgt ->
  Data.Patched_exe.get_patches obj >>= fun patches ->
  Events.(send @@ Info (Printf.sprintf "There are %d patches"
                          (Data.Patch_set.length patches)));

  Data.Patch_set.fold patches
    ~init:(KB.return 1)
    ~f:(ingest_one tgt) >>= fun _ ->

  Events.(send @@ Info "Patch ingest complete");
  Events.(send @@ Rule);
  KB.return ()
