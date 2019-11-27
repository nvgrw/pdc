open Common.VisitorMonad
open PassContext
open Common.AST

module Walker_TypeCheck = Common.Walker.Make(struct 
    type ctx = context

    let visit_program_pre p = success p
    let visit_program_pos p = success p

    let visit_block_pre b = success b
    let visit_block_pos b = success b

    let visit_stmt_pre s = success s
    let visit_stmt_pos s = success s

    let visit_decl_pre d = success d
    let visit_decl_pos d = success d

    let visit_expr_pre e = success e
    let visit_expr_pos = function
      | Const value as v -> (match value with
          | Num _ -> success @@ Typed (Int, v)
          | Real _ -> success @@ Typed (Float, v)
          | Bool _ -> success @@ Typed (Bool, v))
      | _ as e -> success e

    let visit_loc_pre l = success l
    let visit_loc_pos l = success l

    let visit_typ_pre t = success t
    let visit_typ_pos t = success t
  end)

let process = Walker_TypeCheck.walk_program