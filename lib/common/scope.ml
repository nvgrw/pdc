open AST
open VisitorMonad
open Data
open Meta
open Context

module Option = Core.Option

(* Semantic Only *)
let scope_block_pre _ = function
  | Block (scope, _, _, _) as e ->
    get >>= fun wrapped_state -> match wrapped_state with
    | `Semantic state ->
      (* Push scope to state *)
      put @@ `Semantic { S.scopes = scope :: state.S.scopes } >>= fun _ -> 
      success e
    | _ -> assert false
let scope_block_pos _ = function
  | Block (_, decls, stmts, m) ->
    get >>= fun wrapped_state -> match wrapped_state with
    | `Semantic state ->
      put @@ `Semantic { S.scopes = List.tl state.S.scopes } >>= fun _ ->
      (* Pop scope from state *)
      success @@ Block (List.hd state.scopes, decls, stmts, m)
    | _ -> assert false

let rec lookup m ident scopes = match scopes with
  | s :: ss ->
    let ident_type = StringMap.find_opt ident s in
    let state_type = Option.map ~f:(fun t -> success t) ident_type in
    Option.value state_type ~default:(lookup m ident ss)
  | _ -> error @@ StructuralError (BadIdentifier (m, ident))

let get_typ m ident =
  get >>= fun wrapped_state -> match wrapped_state with
  | `Semantic state ->
    lookup m ident state.S.scopes
  | _ -> assert false

let get_llvalue m ident =
  get >>= fun wrapped_state -> match wrapped_state with
  | `Codegen state ->
    lookup m ident state.C.scopes
  | _ -> assert false