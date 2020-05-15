open AST
open VisitorMonad
module Option = Core.Option

module type Visitor = sig
  type ctx
  type err
  type mta

  val visit_program_pre: mta ast -> mta program -> (ctx, mta program, err) state
  val visit_program_pos: mta ast -> mta program -> (ctx, mta program, err) state

  val scope_block_pre: mta ast -> mta block -> (ctx, mta block, err) state
  val scope_block_pos: mta ast -> mta block -> (ctx, mta block, err) state
  val visit_block_pre: mta ast -> mta block -> (ctx, mta block, err) state
  val visit_block_pos: mta ast -> mta block -> (ctx, mta block, err) state

  val visit_stmt_pre: mta ast -> mta stmt -> (ctx, mta stmt, err) state
  val visit_stmt_pos: mta ast -> mta stmt -> (ctx, mta stmt, err) state

  val visit_decl_pre: mta ast -> mta decl -> (ctx, mta decl, err) state
  val visit_decl_pos: mta ast -> mta decl -> (ctx, mta decl, err) state

  val visit_expr_pre: mta ast -> mta expr -> (ctx, mta expr, err) state
  val visit_expr_pos: mta ast -> mta expr -> (ctx, mta expr, err) state

  val visit_loc_pre: mta ast -> mta loc -> (ctx, mta loc, err) state
  val visit_loc_pos: mta ast -> mta loc -> (ctx, mta loc, err) state

  val visit_typ_pre: mta ast -> mta typ -> (ctx, mta typ, err) state
  val visit_typ_pos: mta ast -> mta typ -> (ctx, mta typ, err) state
end

module Make(V : Visitor) = struct
  type ctx = V.ctx
  type err = V.err
  type mta = V.mta

  let rec walk_typ (parent: mta ast) (t: mta typ): (ctx, mta typ, err) state =
    V.visit_typ_pre parent t
    >>= fun pre_result -> begin
      match pre_result with
      | Array (typ, size, m) ->
        walk_typ (`Typ pre_result) typ >>= fun walk_typ_typ ->
        success (Array (walk_typ_typ, size, m))
      | Int _ as t -> success t
      | Float _ as t -> success t
      | Char _ as t -> success t
      | Bool _ as t -> success t
    end
    >>= fun walk_result -> V.visit_typ_pos parent walk_result

  let rec walk_expr (parent: mta ast) (e: mta expr): (ctx, mta expr, err) state =
    V.visit_expr_pre parent e
    >>= fun pre_result -> begin
      match pre_result with
      | BinOp (l, op, r, m) ->
        walk_expr (`Expr pre_result) l >>= fun walk_expr_l ->
        walk_expr (`Expr pre_result) r >>= fun walk_expr_r ->
        success (BinOp (walk_expr_l, op, walk_expr_r, m))
      | UnOp (op, expr, m) ->
        walk_expr (`Expr pre_result) expr >>= fun walk_expr_expr ->
        success (UnOp (op, walk_expr_expr, m))
      | Const _  as e -> success e
      | Var (loc, m) ->
        walk_loc (`Expr pre_result) loc >>= fun walk_loc_loc ->
        success (Var (walk_loc_loc, m))
      | PtrVar (loc, m) ->
        walk_loc (`Expr pre_result) loc >>= fun walk_loc_loc ->
        success (PtrVar (walk_loc_loc, m))
      | Typed (typ, expr, m) ->
        walk_typ (`Expr pre_result) typ >>= fun walk_typ_typ ->
        walk_expr (`Expr pre_result) expr >>= fun walk_expr_expr ->
        success (Typed (walk_typ_typ, walk_expr_expr, m))
    end
    >>= fun walk_result -> V.visit_expr_pos parent walk_result

  and walk_loc (parent: mta ast) (l: mta loc): (ctx, mta loc, err) state =
    V.visit_loc_pre parent l
    >>= fun pre_result -> begin
      match pre_result with
      | Id _ as l -> success l
      | Deref (loc, expr, m) ->
        walk_loc (`Loc pre_result) loc >>= fun walk_loc_loc ->
        walk_expr (`Loc pre_result) expr >>= fun walk_expr_expr ->
        success (Deref (walk_loc_loc, walk_expr_expr, m))
      | LTyped (typ, loc, m) ->
        walk_typ (`Loc pre_result) typ >>= fun walk_typ_typ ->
        walk_loc (`Loc pre_result) loc >>= fun walk_loc_loc ->
        success (LTyped (walk_typ_typ, walk_loc_loc, m))
    end
    >>= fun walk_result -> V.visit_loc_pos parent walk_result

  let walk_decl (parent: mta ast) (d: mta decl): (ctx, mta decl, err) state =
    V.visit_decl_pre parent d
    >>= fun pre_result -> begin
      match pre_result with
      | Decl (typ, id, m) ->
        walk_typ (`Decl pre_result) typ >>= fun walk_typ_typ ->
        success (Decl (walk_typ_typ, id, m))
    end
    >>= fun walk_result -> V.visit_decl_pos parent walk_result

  let rec walk_stmt (parent: mta ast) (s: mta stmt): (ctx, mta stmt, err) state =
    V.visit_stmt_pre parent s
    >>= fun pre_result -> begin
      match pre_result with
      | Assign (loc, expr, m) ->
        walk_loc (`Stmt pre_result) loc >>= fun walk_loc_loc ->
        walk_expr (`Stmt pre_result) expr >>= fun walk_expr_expr ->
        success (Assign (walk_loc_loc, walk_expr_expr, m))
      | ProbAssign (loc, exprs, m) ->
        walk_loc (`Stmt pre_result) loc >>= fun walk_loc_loc ->
        seqList (List.map (walk_expr (`Stmt pre_result)) exprs) >>= fun walk_expr_exprs ->
        success (ProbAssign (walk_loc_loc, walk_expr_exprs, m))
      | If (expr, stmt, stmt_opt, m) ->
        walk_expr (`Stmt pre_result) expr >>= fun walk_expr_expr ->
        walk_stmt (`Stmt pre_result) stmt >>= fun walk_stmt_stmt ->
        seqOpt (Option.map ~f:(walk_stmt (`Stmt pre_result)) stmt_opt) >>= fun walk_stmt_stmt_opt ->
        success (If (walk_expr_expr, walk_stmt_stmt, walk_stmt_stmt_opt, m))
      | While (expr, stmt, m) ->
        walk_expr (`Stmt pre_result) expr >>= fun walk_expr_expr ->
        walk_stmt (`Stmt pre_result) stmt >>= fun walk_stmt_stmt ->
        success (While (walk_expr_expr, walk_stmt_stmt, m))
      | Do (expr, stmt, m) ->
        walk_stmt (`Stmt pre_result) stmt >>= fun walk_stmt_stmt ->
        walk_expr (`Stmt pre_result) expr >>= fun walk_expr_expr ->
        success (Do (walk_expr_expr, walk_stmt_stmt, m))
      | Break _ as s -> success s
      | Print (expr, m) ->
        walk_expr (`Stmt pre_result) expr >>= fun walk_expr_expr ->
        success (Print (walk_expr_expr, m))
      | Choose (stmts, probs, m) ->
        seqList (List.map (walk_stmt (`Stmt pre_result)) stmts) >>= fun walk_stmt_stmts ->
        success (Choose (walk_stmt_stmts, probs, m))
      | BlockStmt (block, m) ->
        walk_block (`Stmt pre_result) block >>= fun walk_block_block ->
        success (BlockStmt (walk_block_block, m))
    end
    >>= fun walk_result -> V.visit_stmt_pos parent walk_result

  and walk_block (parent: mta ast) (b: mta block): (ctx, mta block, err) state =
    V.scope_block_pre parent b >>=
    V.visit_block_pre parent
    >>= fun pre_result -> begin
      match pre_result with
      | Block (scope, decls, stmts, m) ->
        seqList (List.map (walk_decl (`Block pre_result)) decls) >>= fun walk_decl_decls ->
        seqList (List.map (walk_stmt (`Block pre_result)) stmts) >>= fun walk_stmt_stmts ->
        success (Block (scope, walk_decl_decls, walk_stmt_stmts, m))
    end
    >>= fun walk_result -> V.visit_block_pos parent walk_result
    >>= V.scope_block_pos parent

  let walk_program parent p =
    V.visit_program_pre parent p
    >>= fun pre_result -> walk_block (`Program p) pre_result
    >>= fun walk_result -> V.visit_program_pos parent walk_result
end