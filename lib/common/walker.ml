open AST
open VisitorMonad

module type Visitor = sig 
  type ctx
  type err
  type mta

  val visit_program_pre: mta program -> (ctx, mta program, err) state
  val visit_program_pos: mta program -> (ctx, mta program, err) state

  val scope_block_pre: mta block -> (ctx, mta block, err) state
  val scope_block_pos: mta block -> (ctx, mta block, err) state
  val visit_block_pre: mta block -> (ctx, mta block, err) state
  val visit_block_pos: mta block -> (ctx, mta block, err) state

  val visit_stmt_pre: mta stmt -> (ctx, mta stmt, err) state
  val visit_stmt_pos: mta stmt -> (ctx, mta stmt, err) state

  val visit_decl_pre: mta decl -> (ctx, mta decl, err) state
  val visit_decl_pos: mta decl -> (ctx, mta decl, err) state

  val visit_expr_pre: mta expr -> (ctx, mta expr, err) state
  val visit_expr_pos: mta expr -> (ctx, mta expr, err) state

  val visit_loc_pre: mta loc -> (ctx, mta loc, err) state
  val visit_loc_pos: mta loc -> (ctx, mta loc, err) state

  val visit_typ_pre: mta typ -> (ctx, mta typ, err) state
  val visit_typ_pos: mta typ -> (ctx, mta typ, err) state
end

module Make(V : Visitor) = struct 
  type ctx = V.ctx
  type err = V.err
  type mta = V.mta

  let rec walk_typ (t: mta typ): (ctx, mta typ, err) state = 
    V.visit_typ_pre t 
    >>= fun pre_result -> (match pre_result with
        | Array (typ, size, m) -> 
          walk_typ typ >>= fun walk_typ_typ -> 
          success (Array (walk_typ_typ, size, m))
        | Int _ as t -> success t
        | Float _ as t -> success t
        | Char _ as t -> success t
        | Bool _ as t -> success t)
    >>= fun walk_result -> V.visit_typ_pos walk_result

  let rec walk_expr (e: mta expr): (ctx, mta expr, err) state = 
    V.visit_expr_pre e
    >>= fun pre_result -> (match pre_result with
        | BinOp (l, op, r, m) -> 
          walk_expr l >>= fun walk_expr_l -> 
          walk_expr r >>= fun walk_expr_r ->
          success (BinOp (walk_expr_l, op, walk_expr_r, m))
        | UnOp (op, expr, m) -> 
          walk_expr expr >>= fun walk_expr_expr ->
          success (UnOp (op, walk_expr_expr, m))
        | Const _  as e -> success e
        | Var (loc, m) -> 
          walk_loc loc >>= fun walk_loc_loc ->
          success (Var (walk_loc_loc, m))
        | Typed (typ, expr, m) -> 
          walk_typ typ >>= fun walk_typ_typ -> 
          walk_expr expr >>= fun walk_expr_expr ->
          success (Typed (walk_typ_typ, walk_expr_expr, m)))
    >>= fun walk_result -> V.visit_expr_pos walk_result

  and walk_loc (l: mta loc): (ctx, mta loc, err) state = 
    V.visit_loc_pre l 
    >>= fun pre_result -> (match pre_result with
        | Id _ as l -> success l
        | Deref (loc, expr, m) -> 
          walk_loc loc >>= fun walk_loc_loc -> 
          walk_expr expr >>= fun walk_expr_expr ->
          success (Deref (walk_loc_loc, walk_expr_expr, m))
        | LTyped (typ, loc, m) ->
          walk_typ typ >>= fun walk_typ_typ ->
          walk_loc loc >>= fun walk_loc_loc ->
          success (LTyped (walk_typ_typ, walk_loc_loc, m))
      )
    >>= fun walk_result -> V.visit_loc_pos walk_result

  let walk_decl (d: mta decl): (ctx, mta decl, err) state = 
    V.visit_decl_pre d 
    >>= fun pre_result -> (match pre_result with
        | Decl (typ, id, m) -> 
          walk_typ typ >>= fun walk_typ_typ ->
          success (Decl (walk_typ_typ, id, m)))
    >>= fun walk_result -> V.visit_decl_pos walk_result

  let rec walk_stmt (s: mta stmt): (ctx, mta stmt, err) state = 
    V.visit_stmt_pre s 
    >>= fun pre_result -> (match pre_result with
        | Assign (loc, expr, m) -> 
          walk_loc loc >>= fun walk_loc_loc ->
          walk_expr expr >>= fun walk_expr_expr ->
          success (Assign (walk_loc_loc, walk_expr_expr, m))
        | If (expr, stmt, stmt_opt, m) -> 
          walk_expr expr >>= fun walk_expr_expr ->
          walk_stmt stmt >>= fun walk_stmt_stmt ->
          seqOpt (Option.map walk_stmt stmt_opt) >>= fun walk_stmt_stmt_opt ->
          success (If (walk_expr_expr, walk_stmt_stmt, walk_stmt_stmt_opt, m))
        | While (expr, stmt, m) -> 
          walk_expr expr >>= fun walk_expr_expr ->
          walk_stmt stmt >>= fun walk_stmt_stmt ->
          success (While (walk_expr_expr, walk_stmt_stmt, m))
        | Do (expr, stmt, m) -> 
          walk_expr expr >>= fun walk_expr_expr ->
          walk_stmt stmt >>= fun walk_stmt_stmt ->
          success (Do (walk_expr_expr, walk_stmt_stmt, m))
        | Break _ as s -> success s
        | BlockStmt (block, m) -> 
          walk_block block >>= fun walk_block_block ->
          success (BlockStmt (walk_block_block, m)))
    >>= fun walk_result -> V.visit_stmt_pos walk_result

  and walk_block (b: mta block): (ctx, mta block, err) state = 
    V.scope_block_pre b >>=
    V.visit_block_pre
    >>= fun pre_result -> (match pre_result with
        | Block (scope, decls, stmts, m) -> 
          seqList (List.map walk_decl decls) >>= fun walk_decl_decls ->
          seqList (List.map walk_stmt stmts) >>= fun walk_stmt_stmts ->
          success (Block (scope, walk_decl_decls, walk_stmt_stmts, m))
      )
    >>= fun walk_result -> V.visit_block_pos walk_result 
    >>= V.scope_block_pos

  let walk_program p = 
    V.visit_program_pre p 
    >>= fun pre_result -> walk_block pre_result
    >>= fun walk_result -> V.visit_program_pos walk_result
end