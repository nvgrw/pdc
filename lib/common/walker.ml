open AST
open VisitorMonad

module type Visitor = sig 
  type ctx
  type err

  val visit_program_pre: program -> (ctx, program, err) state
  val visit_program_pos: program -> (ctx, program, err) state

  val scope_block_pre: block -> (ctx, block, err) state
  val scope_block_pos: block -> (ctx, block, err) state
  val visit_block_pre: block -> (ctx, block, err) state
  val visit_block_pos: block -> (ctx, block, err) state

  val visit_stmt_pre: stmt -> (ctx, stmt, err) state
  val visit_stmt_pos: stmt -> (ctx, stmt, err) state

  val visit_decl_pre: decl -> (ctx, decl, err) state
  val visit_decl_pos: decl -> (ctx, decl, err) state

  val visit_expr_pre: expr -> (ctx, expr, err) state
  val visit_expr_pos: expr -> (ctx, expr, err) state

  val visit_loc_pre: loc -> (ctx, loc, err) state
  val visit_loc_pos: loc -> (ctx, loc, err) state

  val visit_typ_pre: typ -> (ctx, typ, err) state
  val visit_typ_pos: typ -> (ctx, typ, err) state
end

module Make(V : Visitor) = struct 
  type ctx = V.ctx
  type err = V.err

  let rec walk_typ (t: typ): (ctx, typ, err) state = 
    V.visit_typ_pre t 
    >>= fun pre_result -> (match pre_result with
        | Array (typ, size) -> 
          walk_typ typ >>= fun walk_typ_typ -> 
          success (Array (walk_typ_typ, size))
        | Int -> success Int
        | Float -> success Float
        | Char -> success Char
        | Bool -> success Bool)
    >>= fun walk_result -> V.visit_typ_pos walk_result

  let rec walk_expr (e: expr): (ctx, expr, err) state = 
    V.visit_expr_pre e
    >>= fun pre_result -> (match pre_result with
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
          success (Typed (walk_typ_typ, walk_expr_expr)))
    >>= fun walk_result -> V.visit_expr_pos walk_result

  and walk_loc (l: loc): (ctx, loc, err) state = 
    V.visit_loc_pre l 
    >>= fun pre_result -> (match pre_result with
        | Id id -> success (Id id)
        | Deref (loc, expr) -> 
          walk_loc loc >>= fun walk_loc_loc -> 
          walk_expr expr >>= fun walk_expr_expr ->
          success (Deref (walk_loc_loc, walk_expr_expr)))
    >>= fun walk_result -> V.visit_loc_pos walk_result

  let walk_decl (d: decl): (ctx, decl, err) state = 
    V.visit_decl_pre d 
    >>= fun pre_result -> (match pre_result with
        | Decl (typ, id) -> 
          walk_typ typ >>= fun walk_typ_typ ->
          success (Decl (walk_typ_typ, id)))
    >>= fun walk_result -> V.visit_decl_pos walk_result

  let rec walk_stmt (s: stmt): (ctx, stmt, err) state = 
    V.visit_stmt_pre s 
    >>= fun pre_result -> (match pre_result with
        | Assign (loc, expr) -> 
          walk_loc loc >>= fun walk_loc_loc ->
          walk_expr expr >>= fun walk_expr_expr ->
          success (Assign (walk_loc_loc, walk_expr_expr))
        | If (expr, stmt, stmt_opt) -> 
          walk_expr expr >>= fun walk_expr_expr ->
          walk_stmt stmt >>= fun walk_stmt_stmt ->
          seqOpt (Option.map walk_stmt stmt_opt) >>= fun walk_stmt_stmt_opt ->
          success (If (walk_expr_expr, walk_stmt_stmt, walk_stmt_stmt_opt))
        | While (expr, stmt) -> 
          walk_expr expr >>= fun walk_expr_expr ->
          walk_stmt stmt >>= fun walk_stmt_stmt ->
          success (While (walk_expr_expr, walk_stmt_stmt))
        | Do (expr, stmt) -> 
          walk_expr expr >>= fun walk_expr_expr ->
          walk_stmt stmt >>= fun walk_stmt_stmt ->
          success (Do (walk_expr_expr, walk_stmt_stmt))
        | Break -> success Break
        | BlockStmt block -> 
          walk_block block >>= fun walk_block_block ->
          success (BlockStmt (walk_block_block)))
    >>= fun walk_result -> V.visit_stmt_pos walk_result

  and walk_block (b: block): (ctx, block, err) state = 
    V.scope_block_pre b >>=
    V.visit_block_pre
    >>= fun pre_result -> (match pre_result with
        | Block (scope, decls, stmts) -> 
          seqList (List.map walk_decl decls) >>= fun walk_decl_decls ->
          seqList (List.map walk_stmt stmts) >>= fun walk_stmt_stmts ->
          success (Block (scope, walk_decl_decls, walk_stmt_stmts))
      )
    >>= fun walk_result -> V.visit_block_pos walk_result 
    >>= V.scope_block_pos

  let walk_program p = 
    V.visit_program_pre p 
    >>= fun pre_result -> walk_block pre_result
    >>= fun walk_result -> V.visit_program_pos walk_result
end