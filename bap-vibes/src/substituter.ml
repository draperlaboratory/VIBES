(** Implements {!Substituter}. *)

open !Core_kernel

module KB = Bap_knowledge.Knowledge
module Hvar = Higher_var

open KB.Let

let err msg = Kb_error.fail @@ Kb_error.Higher_vars_not_substituted msg
let str_of = Sexp.to_string

let subst_in_decls (h_vars : Hvar.t list) (decls : Sexp.t) : Sexp.t KB.t =
  let* vars = match decls with
    | Sexp.List ((Sexp.Atom "var-decls") :: vars) -> KB.return vars
    | Sexp.Atom "var-decls" -> KB.return []
    | _ -> err "invalid or missing var-decls"
  in
  let filtered_vars = List.filter vars ~f:(fun sexp ->
    match sexp with
    | Sexp.Atom v ->
      begin
        match Hvar.find v h_vars with
        | Some h_var ->
          let at_entry = Hvar.at_entry h_var in
          Hvar.is_reg at_entry
        | None -> true
      end
    | _ -> false)
  in
  let* new_vars = KB.all @@ List.map filtered_vars ~f:(fun sexp ->
    match sexp with
    | Sexp.Atom v ->
      begin
        match Hvar.find v h_vars with
        | Some h_var ->
          let at_entry = Hvar.at_entry h_var in
          if Hvar.is_reg at_entry then 
            KB.return @@ Hvar.sexp_of at_entry
          else 
            err @@ Format.sprintf
              "uh-oh, '%s' should have been removed" (str_of sexp)
        | None -> KB.return sexp
      end
    | _ -> err @@ Format.sprintf "invalid var-decl '%s'" (str_of sexp))
  in
  KB.return @@ Sexp.List (Sexp.Atom "var-decls" :: new_vars)

let rec subst_in_expr (h_vars : Hvar.t list) (expr : Sexp.t) : Sexp.t KB.t =
  match expr with
  | Sexp.Atom s ->
    begin
      match Hvar.find s h_vars with
      | Some h_var ->
        let at_entry = Hvar.at_entry h_var in
        KB.return @@ Hvar.sexp_of at_entry
      | None -> KB.return expr
    end
  | Sexp.List [Sexp.Atom "load"; src] ->
    let* new_src = subst_in_expr h_vars src in
    KB.return @@ Sexp.List [Sexp.Atom "load"; new_src]
  | Sexp.List [Sexp.Atom "loadw"; Sexp.Atom bits; src] ->
    let* new_src = subst_in_expr h_vars src in
    KB.return @@ Sexp.List [Sexp.Atom "loadw"; Sexp.Atom bits; new_src]
  | Sexp.List [Sexp.Atom "+"; e1; e2] ->
    let* new_e1 = subst_in_expr h_vars e1 in
    let* new_e2 = subst_in_expr h_vars e2 in
    KB.return @@ Sexp.List [Sexp.Atom "+"; new_e1; new_e2]
  | Sexp.List [Sexp.Atom "-"; e1; e2] ->
    let* new_e1 = subst_in_expr h_vars e1 in
    let* new_e2 = subst_in_expr h_vars e2 in
    KB.return @@ Sexp.List [Sexp.Atom "-"; new_e1; new_e2]
  | Sexp.List [Sexp.Atom "*"; e1; e2] ->
    let* new_e1 = subst_in_expr h_vars e1 in
    let* new_e2 = subst_in_expr h_vars e2 in
    KB.return @@ Sexp.List [Sexp.Atom "*"; new_e1; new_e2]
  | Sexp.List [Sexp.Atom "/"; e1; e2] ->
    let* new_e1 = subst_in_expr h_vars e1 in
    let* new_e2 = subst_in_expr h_vars e2 in
    KB.return @@ Sexp.List [Sexp.Atom "/"; new_e1; new_e2]
  | Sexp.List [Sexp.Atom ">>"; e1; e2] ->
    let* new_e1 = subst_in_expr h_vars e1 in
    let* new_e2 = subst_in_expr h_vars e2 in
    KB.return @@ Sexp.List [Sexp.Atom ">>"; new_e1; new_e2]
  | _ -> err @@ Format.sprintf "invalid expression: '%s'" (str_of expr)

let subst_in_var_assignment
    (h_vars : Hvar.t list) (dest : string) (expr : Sexp.t) : Sexp.t KB.t =
  let* new_expr = subst_in_expr h_vars expr in
  match Hvar.find dest h_vars with
  | Some h_var ->
    let at_entry = Hvar.at_entry h_var in
    if Hvar.is_reg at_entry then
      let new_dest = Hvar.sexp_of at_entry in
      KB.return @@ Sexp.List [Sexp.Atom "set"; new_dest; new_expr]
    else
      let loc = Hvar.sexp_of_loc at_entry in
      let value =
        Sexp.List [Sexp.Atom "store"; Sexp.Atom "mem"; loc; new_expr]
      in
      KB.return @@ Sexp.List [Sexp.Atom "set"; Sexp.Atom "mem"; value]
  | None -> KB.return @@ Sexp.List [Sexp.Atom "set"; Sexp.Atom dest; new_expr] 

let subst_in_stmt (h_vars : Hvar.t list) (stmt : Sexp.t) : Sexp.t KB.t =
  match stmt with
  | Sexp.List [Sexp.Atom "set"; Sexp.Atom dest; expr] ->
    subst_in_var_assignment h_vars dest expr
  | _ -> err @@ Format.sprintf "invalid statement '%s'" (str_of stmt)

let subst_in_bool (h_vars : Hvar.t list) (expr : Sexp.t) : Sexp.t KB.t =
  match expr with
  | Sexp.List [Sexp.Atom "=="; e1; e2] ->
    let* new_e1 = subst_in_expr h_vars e1 in
    let* new_e2 = subst_in_expr h_vars e2 in
    KB.return @@ Sexp.List [Sexp.Atom "=="; new_e1; new_e2]
  | _ ->
    err @@ Format.sprintf "invalid boolean expression: '%s'" (str_of expr)

let rec subst_in_cmd (h_vars : Hvar.t list) (cmd : Sexp.t) : Sexp.t KB.t =
  match cmd with
  | Sexp.Atom "fallthrough" -> KB.return cmd
  | Sexp.List [Sexp.Atom "branch"; cond; branch1; branch2] ->
    let* new_cond = subst_in_bool h_vars cond in
    let* new_branch1 = subst_in_cmd h_vars branch1 in
    let* new_branch2 = subst_in_cmd h_vars branch2 in
    KB.return @@
      Sexp.List [Sexp.Atom "branch"; new_cond; new_branch1; new_branch2]
  | Sexp.List [Sexp.Atom "jmp"; dest] ->
    let* new_dest = subst_in_expr h_vars dest in
    KB.return @@ Sexp.List [Sexp.Atom "jmp"; new_dest]
  | _ -> err @@ Format.sprintf "invalid command: '%s'" (str_of cmd)

let substitute
    (h_vars : Hvar.t list) (sexps : Sexp.t list) : Sexp.t list KB.t =
  match sexps with
  | [] -> err "empty patch code"
  | decls :: sexps ->
    begin
      match List.split_n sexps (List.length sexps - 1) with
      | (stmts, [cmd]) ->
        let* new_decls = subst_in_decls h_vars decls in
        let* new_stmts = KB.all
          (List.map stmts ~f:(fun stmt -> subst_in_stmt h_vars stmt))
        in
        let* new_cmd = subst_in_cmd h_vars cmd in
        KB.return @@ new_decls :: (List.append new_stmts [new_cmd])
      | _ -> err "invalid patch code, expecting statements and command"
    end
