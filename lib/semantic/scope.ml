open Common.AST
open Common.VisitorMonad
open Common.Data
open Common.Meta

open Context

module Option = Core.Option

let scope_block_pre = function
  | Block (scope, _, _, _) as e ->
    get >>= fun state ->
    (* Push scope to state *)
    put { scopes = scope :: state.scopes } >>= fun _ -> 
    success e
let scope_block_pos = function
  | Block (_, decls, stmts, m) ->
    get >>= fun state ->
    put { scopes = List.tl state.scopes } >>= fun _ ->
    (* Pop scope from state *)
    success @@ Block (List.hd state.scopes, decls, stmts, m)

let rec lookup m ident scopes = match scopes with
  | s :: ss ->
    let ident_type = StringMap.find_opt ident s in
    let state_type = Option.map ~f:(fun t -> success t) ident_type in
    Option.value state_type ~default:(lookup m ident ss)
  | _ -> error @@ StructuralError (BadIdentifier (m, ident))

let get_type m ident = 
  get >>= fun state ->
  lookup m ident state.scopes
