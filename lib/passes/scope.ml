open Common.AST
open Common.VisitorMonad
open PassContext
open Common.Data

let scope_block_pre = function
  | Block (scope, _, _, _) as e ->
    get >>= fun state ->
    (* Push scope to state *)
    put { state with scopes = scope :: state.scopes } >>= fun _ -> 
    success e
let scope_block_pos = function
  | Block (_, decls, stmts, m) ->
    get >>= fun state ->
    put { state with scopes = List.tl state.scopes } >>= fun _ ->
    (* Pop scope from state *)
    success @@ Block (List.hd state.scopes, decls, stmts, m)

let rec lookup ident scopes = match scopes with
  | s :: ss ->
    let ident_type = StringMap.find_opt ident s in
    let state_type = Option.map (fun t -> success t) ident_type in
    Option.value state_type ~default:(lookup ident ss)
  | _ -> error @@ StructuralError (BadIdentifier ident)

let get_type ident = 
  get >>= fun state ->
  lookup ident state.scopes
