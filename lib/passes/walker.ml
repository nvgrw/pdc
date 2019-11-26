open AST

(* Result type for visitor *)
type ('state, 'ast) res =
  | Success of 'state * 'ast
  | Error 

(* State monad for visitor *)
type ('state, 'ast) state = 'state -> ('state, 'ast) res

let (>>=) m f =
  fun s -> 
  match m s with
  | Error -> Error
  | Success (t, v) -> (f v) t

let success v = fun s -> Success (s, v)
let error = fun _ -> Error
let map m f = m >>= fun v -> success (f v)

let seqOpt (s: ('state, 'v option) state): ('state, 'v) state option = ()

(*
val (>>=): ('s, 'a) state -> ('a -> ('s, 'b) state) -> ('s, 'b) state
val success: 'a -> ('s, 'a) state
val error: ('s, 'a) state
*)

module type Visitor = sig 
  type ctx

  val visit_program_pre: program -> (ctx, program) state
  val visit_program_pos: program -> (ctx, program) state

  val visit_block_pre: block -> (ctx, block) state
  val visit_block_pos: block -> (ctx, block) state

  val visit_stmt_pre: stmt -> (ctx, stmt) state
  val visit_stmt_pos: stmt -> (ctx, stmt) state

  val visit_decl_pre: decl -> (ctx, decl) state
  val visit_decl_pos: decl -> (ctx, decl) state

  val visit_expr_pre: expr -> (ctx, expr) state
  val visit_expr_pos: expr -> (ctx, expr) state

  val visit_loc_pre: loc -> (ctx, loc) state
  val visit_loc_pos: loc -> (ctx, loc) state

  val visit_typ_pre: typ -> (ctx, typ) state
  val visit_typ_pos: typ -> (ctx, typ) state
end

module Make(V : Visitor) = struct 
  type ctx = V.ctx

  let rec walk_typ (t: typ): (ctx, typ) state = 
    V.visit_typ_pre t 
    >>= fun pre_result -> match pre_result with
    | Array (typ, size) -> 
      walk_typ typ >>= fun walk_typ_typ -> 
      success (Array (walk_typ_typ, size))
    | Int -> success Int
    | Float -> success Float
    | Char -> success Char
    | Bool -> success Bool
      >>= fun walk_result -> V.visit_typ_pos walk_result

  let rec walk_expr (e: expr): (ctx, expr) state = 
    V.visit_expr_pre e
    >>= fun pre_result -> match pre_result with
    | BinOp (l, op, r) -> 
      walk_expr l >>= fun walk_expr_l -> 
      walk_expr r >>= fun walk_expr_r ->
      success (BinOp (walk_expr_l, op, walk_expr_r))
    | UnOp (op, expr) -> 
      walk_expr expr >>= fun walk_expr_expr ->
      success (UnOp (op, walk_expr_expr))
    | Const value -> success (Const value)
    | Var loc -> 
      walk_loc loc >>= fun walk_loc_loc ->
      success (Var (walk_loc_loc))
    | Typed (typ, expr) -> 
      walk_typ typ >>= fun walk_typ_typ -> 
      walk_expr expr >>= fun walk_expr_expr ->
      success (Typed (walk_typ_typ, walk_expr_expr))
      >>= fun walk_result -> V.visit_expr_pos walk_result

  and walk_loc (l: loc): (ctx, loc) state = 
    V.visit_loc_pre l 
    >>= fun pre_result -> match pre_result with
    | Id id -> success (Id id)
    | Deref (loc, expr) -> 
      walk_loc loc >>= fun walk_loc_loc -> 
      walk_expr expr >>= fun walk_expr_expr ->
      success (Deref (walk_loc_loc, walk_expr_expr))
      >>= fun walk_result -> V.visit_loc_pos walk_result

  let walk_decl (d: decl): (ctx, decl) state = 
    V.visit_decl_pre d 
    >>= fun pre_result -> match pre_result with
    | Decl (typ, id) -> 
      walk_typ typ >>= fun walk_typ_typ ->
      success (Decl (walk_typ_typ, id))
      >>= fun walk_result -> V.visit_decl_pos walk_result

  let rec walk_stmt (s: stmt): (ctx, stmt) state = 
    V.visit_stmt_pre s 
    >>= fun pre_result -> match pre_result with
    | Assign (loc, expr) -> 
      walk_loc loc >>= fun walk_loc_loc ->
      walk_expr expr >>= fun walk_expr_expr ->
      success (Assign (walk_loc_loc, walk_expr_expr))
    | If (expr, stmt, stmt_opt) -> 
      walk_expr expr >>= fun walk_expr_expr ->
      walk_stmt stmt >>= fun walk_stmt_stmt ->
      success (If (walk_expr_expr, walk_stmt_stmt, Option.map walk_stmt stmt_opt))
    | While (expr, stmt) -> While (walk_expr expr, walk_stmt stmt)
    | Do (expr, stmt) -> Do (walk_expr expr, walk_stmt stmt)
    | Break -> Break
    | BlockStmt block -> BlockStmt (walk_block block)
      >>= fun walk_result -> V.visit_stmt_pos walk_result

  and walk_block b = 
    let pre_result = V.visit_block_pre b in 
    let walk_result = match pre_result with
      | Block (decls, stmts) -> Block (List.map walk_decl decls, List.map walk_stmt stmts)
    in V.visit_block_pos walk_result

  let walk_program p s = walk_block p
  (* let pre_result = V.visit_program_pre p s in
     let walk_result = walk_block p in
     in V.visit_program_pos p s *)
end