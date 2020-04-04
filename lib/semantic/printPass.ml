open Common.VisitorMonad
open Common.AST
open Common.Data
open Common.Meta
open Common.Context

module Walker_PrintPass = Common.Walker.Make(struct
    type ctx = context
    type err = pass_error
    type mta = meta

    let visit_program_pre _ p = print_endline "Pre Program"; success p
    let visit_program_pos _ p = print_endline "Pos Program"; success p

    let scope_block_pre _ b = print_endline "SCOPE Pre Block"; success b
    let scope_block_pos _ b = print_endline "SCOPE Pos Block"; success b
    let visit_block_pre _ b =
      print_endline "Pre Block";
      match b with Block (scope, _, _, _) -> StringMap.iter (fun k v -> print_endline @@ Printf.sprintf "%s %s" k (show_typ pp_meta v)) scope;
        success b
    let visit_block_pos _ b = print_endline "Pos Block"; success b

    let visit_stmt_pre _ s = print_endline "Pre Stmt"; success s
    let visit_stmt_pos _ s = print_endline "Pos Stmt"; success s

    let visit_decl_pre _ d = print_endline "Pre Decl"; success d
    let visit_decl_pos _ d = print_endline "Pos Decl"; success d

    let visit_expr_pre _ e = print_endline "Pre Expr"; success e
    let visit_expr_pos _ e = print_endline "Pos Expr"; success e

    let visit_loc_pre _ l = print_endline "Pre Loc"; success l
    let visit_loc_pos _ l = print_endline "Pos Loc"; success l

    let visit_typ_pre _ t = print_endline "Pre Typ"; success t
    let visit_typ_pos _ t = print_endline "Pos Typ"; success t
  end)

let process = Walker_PrintPass.walk_program `Root