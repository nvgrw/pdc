open Common.VisitorMonad
open Common.AST
open Common.Data
open Common.Meta

open Context

module Walker_PrintPass = Common.Walker.Make(struct 
    type ctx = context
    type err = pass_error
    type mta = meta

    let visit_program_pre p = print_endline "Pre Program"; success p
    let visit_program_pos p = print_endline "Pos Program"; success p

    let scope_block_pre b = print_endline "SCOPE Pre Block"; success b
    let scope_block_pos b = print_endline "SCOPE Pos Block"; success b
    let visit_block_pre b = 
      print_endline "Pre Block"; 
      match b with Block (scope, _, _, _) -> StringMap.iter (fun k v -> print_endline @@ Printf.sprintf "%s %s" k (show_typ pp_meta v)) scope;
        success b
    let visit_block_pos b = print_endline "Pos Block"; success b

    let visit_stmt_pre s = print_endline "Pre Stmt"; success s
    let visit_stmt_pos s = print_endline "Pos Stmt"; success s

    let visit_decl_pre d = print_endline "Pre Decl"; success d
    let visit_decl_pos d = print_endline "Pos Decl"; success d

    let visit_expr_pre e = print_endline "Pre Expr"; success e
    let visit_expr_pos e = print_endline "Pos Expr"; success e

    let visit_loc_pre l = print_endline "Pre Loc"; success l
    let visit_loc_pos l = print_endline "Pos Loc"; success l

    let visit_typ_pre t = print_endline "Pre Typ"; success t
    let visit_typ_pos t = print_endline "Pos Typ"; success t
  end)

let process = Walker_PrintPass.walk_program