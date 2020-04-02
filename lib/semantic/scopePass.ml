open Common.VisitorMonad
open Common.AST
open Common.Data
open Common.Meta
open Common.Context

module Scope = Common.Scope

module Walker_ScopePass = Common.Walker.Make(struct
    type ctx = context
    type err = pass_error
    type mta = meta

    let visit_program_pre _ p = success p
    let visit_program_pos _ p = success p

    let scope_block_pre = Scope.scope_block_pre
    let scope_block_pos = Scope.scope_block_pos
    let visit_block_pre _ b = success b
    let visit_block_pos _ b = success b

    let visit_stmt_pre _ s = success s
    let visit_stmt_pos _ s = success s

    let visit_decl_pre _ d = success d
    let visit_decl_pos _ = function
      | Decl (typ, ident, _) as d ->
        get >>= function
        | `Semantic state ->
          let curr = List.hd state.S.scopes in
          begin
            match StringMap.find_opt ident curr with
            | Some _ -> error @@ StructuralError (DuplicateIdentifier (d, ident))
            | None ->
              let new_scope = StringMap.add ident typ @@ curr in
              let new_state = `Semantic { S.scopes = new_scope :: List.tl state.S.scopes } in
              put new_state >>= fun _ ->
              success d
          end
        | _ -> assert false

    let visit_expr_pre _ e = success e
    let visit_expr_pos _ e = success e

    let visit_loc_pre _ l = success l
    let visit_loc_pos _ l = success l

    let visit_typ_pre _ t = success t
    let visit_typ_pos _ t = success t
  end)

let process = Walker_ScopePass.walk_program `Root