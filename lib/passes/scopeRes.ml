open Common.VisitorMonad
open PassContext
open Common.AST
open Common.Data

module Walker_ScopeRes = Common.Walker.Make(struct 
    type ctx = context
    type err = pass_error

    let visit_program_pre p = success p
    let visit_program_pos p = success p

    let visit_block_pre = function
      | Block (scope, _, _) as e ->
        get >>= fun state ->
        (* Push scope to state *)
        put { state with scopes = scope :: state.scopes } >>= fun _ -> 
        success e
    let visit_block_pos = function
      | Block (_, decls, stmts) ->
        get >>= fun state ->
        put { state with scopes = List.tl state.scopes } >>= fun _ ->
        (* Pop scope from state *)
        success @@ Block (List.hd state.scopes, decls, stmts)

    let visit_stmt_pre s = success s
    let visit_stmt_pos s = success s

    let visit_decl_pre d = success d
    let visit_decl_pos = function
      | Decl (typ, ident) as e -> 
        get >>= fun state ->
        let new_scope = StringMap.add ident typ @@ List.hd state.scopes in
        let new_state = { state with scopes = new_scope :: List.tl state.scopes } in
        put new_state >>= fun _ ->
        success e

    let visit_expr_pre e = success e
    let visit_expr_pos e = success e

    let visit_loc_pre l = success l
    let visit_loc_pos l = success l

    let visit_typ_pre t = success t
    let visit_typ_pos t = success t
  end)

let process = Walker_ScopeRes.walk_program