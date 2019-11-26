open AST

module type Visitor = sig 
  val visit_program_pre: program -> program
  val visit_program_pos: program -> program

  val visit_block_pre: block -> block
  val visit_block_pos: block -> block

  val visit_stmt_pre: stmt -> stmt
  val visit_stmt_pos: stmt -> stmt

  val visit_decl_pre: decl -> decl
  val visit_decl_pos: decl -> decl

  val visit_binop: binop -> binop

  val visit_unop: unop -> unop

  val visit_value: value -> value

  val visit_expr_pre: expr -> expr
  val visit_expr_pos: expr -> expr

  val visit_loc_pre: loc -> loc
  val visit_loc_pos: loc -> loc

  val visit_typ_pre: typ -> typ
  val visit_typ_pos: typ -> typ
end

module Make(V : Visitor) = struct 
  let walk_binop b = V.visit_binop b

  let walk_unop u = V.visit_unop u

  let walk_value v = V.visit_value v

  let rec walk_expr e = 
    let pre_result = V.visit_expr_pre e in
    let walk_result = match pre_result with
      | BinOp (l, op, r) -> BinOp (walk_expr l, walk_binop op, walk_expr r)
      | UnOp (op, expr) -> UnOp (walk_unop op, walk_expr expr)
      | Const value -> Const (walk_value value)
      | Var loc -> Var (walk_loc loc)
    in V.visit_expr_pos walk_result

  and walk_loc l = 
    let pre_result = V.visit_loc_pre l in
    let walk_result = match pre_result with
      | Id id -> Id id
      | Deref (loc, expr) -> Deref (walk_loc loc, walk_expr expr)
    in V.visit_loc_pos walk_result

  let rec walk_typ t = 
    let pre_result = V.visit_typ_pre t in
    let walk_result = match pre_result with
      | Array (typ, size) -> Array (walk_typ typ, size)
      | Int -> Int
      | Float -> Float
      | Char -> Char
      | Bool -> Bool
    in V.visit_typ_pos walk_result

  let walk_decl d = 
    let pre_result = V.visit_decl_pre d in
    let walk_result = match pre_result with
      | Decl (typ, id) -> Decl (walk_typ typ, id)
    in V.visit_decl_pos walk_result

  let rec walk_stmt s = 
    let pre_result = V.visit_stmt_pre s in 
    let walk_result = match pre_result with
      | Assign (loc, expr) -> Assign (walk_loc loc, walk_expr expr)
      | If (expr, stmt, stmt_opt) -> If (walk_expr expr, walk_stmt stmt, Option.map walk_stmt stmt_opt)
      | While (expr, stmt) -> While (walk_expr expr, walk_stmt stmt)
      | Do (expr, stmt) -> Do (walk_expr expr, walk_stmt stmt)
      | Break -> Break
      | BlockStmt block -> BlockStmt (walk_block block)
    in V.visit_stmt_pos walk_result

  and walk_block b = 
    let pre_result = V.visit_block_pre b in 
    let walk_result = match pre_result with
      | Block (decls, stmts) -> Block (List.map walk_decl decls, List.map walk_stmt stmts)
    in V.visit_block_pos walk_result

  let walk_program = walk_block
end