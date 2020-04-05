open Common.VisitorMonad
open Common.AST
open Common.Meta
open Common.Context

module Walker_MiscCheckPass = Common.Walker.Make(struct
    type ctx = context
    type err = pass_error
    type mta = meta

    let visit_program_pre _ p = success p
    let visit_program_pos _ p = success p

    let scope_block_pre _ b = success b
    let scope_block_pos _ b = success b
    let visit_block_pre _ b = success b
    let visit_block_pos _ b = success b

    let visit_stmt_pre _ = function
      | Choose (stmts, probs, _)  as s ->
        seqList @@ List.map (function
            | (stmt, 0) -> error @@ StructuralError (ChooseInvalidWeight (stmt, 0))
            | _ -> success ()
          ) (List.combine stmts probs) >>= fun _ ->
        success s
      (* check that all probs are nonzero *)
      | _ as s -> success s
    let visit_stmt_pos _ s = success s

    let visit_decl_pre _ d = success d
    let visit_decl_pos _ d = success d

    let visit_expr_pre _ e = success e
    let visit_expr_pos _ e = success e

    let visit_loc_pre _ l = success l
    let visit_loc_pos _ l = success l

    let visit_typ_pre _ t = success t
    let visit_typ_pos _ t = success t
  end)

let process = Walker_MiscCheckPass.walk_program `Root