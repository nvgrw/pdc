open Common.VisitorMonad
open Common.AST
open Common.Meta
open Common.Context

module Scope = Common.Scope

module Walker_LoweringPass = Common.Walker.Make(struct
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
      | ProbAssign (loc, exprs, m) ->
        let assignments = List.map (fun e -> Assign (loc, e, m)) exprs in
        let weights = List.init (List.length assignments) (Core.const 1) in
        success @@ Choose (assignments, weights, m)
      | Print (Typed (Array _ as arr, Var (loc, m'), m''), m''') ->
        success @@ Print (Typed (arr, PtrVar (loc, m'), m''), m''')
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

let process = Walker_LoweringPass.walk_program `Root