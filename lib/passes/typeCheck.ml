open Common.VisitorMonad
open PassContext
open Common.AST

let unop_compatible = function
  | (Negate _, Int _) | (Negate _, Float _) -> true
  | (Not _, Bool _) -> true
  | _ -> false

let wrap_type typ e = success @@ Typed (typ, e, dummy_meta)

let binop_result = function
  (* Equality *)
  | (t, Eq _, t', m) | (t, Neq _, t', m) when (t == t') -> Some (Bool m)
  (* (Int) Inequality *)
  | (Int _, Lt _, Int _, m) 
  | (Int _, Leq _, Int _, m) 
  | (Int _, Geq _, Int _, m) 
  | (Int _, Gt _, Int _, m) -> Some (Bool m)
  (* (Float) Inequality *)
  | (Float _, Lt _, Float _, m) 
  | (Float _, Leq _, Float _, m) 
  | (Float _, Geq _, Float _, m) 
  | (Float _, Gt _, Float _, m) -> Some (Bool m)
  (* (Bool) Arithmetic *)
  | (Bool _, Or _, Bool _, m) 
  | (Bool _, And _, Bool _, m) -> Some (Bool m)
  (* (Int) Arithmetic *)
  | (Int _, Add _, Int _, m) 
  | (Int _, Subtract _, Int _, m) 
  | (Int _, Multiply _, Int _, m) 
  | (Int _, Divide _, Int _, m) -> Some (Int m)
  (* (Float) Arithmetic *)
  | (Float _, Add _, Float _, m) 
  | (Float _, Subtract _, Float _, m) 
  | (Float _, Multiply _, Float _, m) 
  | (Float _, Divide _, Float _, m) -> Some (Float m)
  | _ -> None

module Walker_TypeCheck = Common.Walker.Make(struct 
    type ctx = context
    type err = pass_error
    type mta = meta

    let visit_program_pre p = success p
    let visit_program_pos p = success p

    let scope_block_pre = Scope.scope_block_pre
    let scope_block_pos = Scope.scope_block_pos
    let visit_block_pre b = success b
    let visit_block_pos b = success b

    let visit_stmt_pre s = success s
    let visit_stmt_pos = function
      | Assign (LTyped(ltyp, _, _), Typed(typ, _, _), _) as s ->
        (* TODO: fix this. cannot check for equality *)
        if ltyp == typ then success s
        else error @@ TypeError (IncompatibleAssignment (ltyp, typ))
      | If (Typed (typ, _, _), _,_, _) as s -> 
        if typ == Bool then success s
        else error @@ TypeError IfRequiresBoolean
      | While (Typed (typ, _, _), _, _) as s ->
        if typ == Bool then success s
        else error @@ TypeError WhileRequiresBoolean
      | Do (Typed (typ, _, _), _, _) as s ->
        if typ == Bool then success s
        else error @@ TypeError DoRequiresBoolean
      | Break _ as s -> success s
      | BlockStmt _ as s -> success s
      | _ as s -> error @@ TypeError (UntypedStatementFragment s)

    let visit_decl_pre d = success d
    let visit_decl_pos d = success d

    let visit_expr_pre e = success e
    let visit_expr_pos = function
      | BinOp (Typed (lt, _), op, Typed (rt, _)) as e -> (
          match binop_result (lt, op, rt) with
          | Some merged_type -> wrap_type merged_type e
          | None -> error @@ TypeError (IncompatibleBinOp (lt, op, rt)))
      | UnOp (op, Typed (typ, _)) as e ->
        if unop_compatible (op, typ)
        then wrap_type typ e
        else error @@ TypeError (IncompatibleUnOp (op, typ))
      | Const (Num _) as v -> success @@ Typed (Int, v)
      | Const (Real _) as v -> success @@ Typed (Float, v)
      | Const (Bool _) as v -> success @@ Typed (Bool, v)
      | Var (LTyped (typ, loc)) -> success @@ Typed (typ, Var loc)
      | Typed _ as typed -> success typed
      | _ as e -> error @@ TypeError (UntypedSubExpressions e)

    let visit_loc_pre l = success l
    let visit_loc_pos = function
      | Deref (LTyped (Array (atyp, _) as arr, loc), expr) -> 
        success @@ LTyped (atyp, Deref (LTyped (arr, loc), expr))
      | Id ident as l -> 
        Scope.get_type ident >>= fun ident_typ ->
        success @@ LTyped (ident_typ, l)
      | LTyped _ as typed -> success typed
      | _ as l  -> error @@ TypeError (UntypedSubLocations l)

    let visit_typ_pre t = success t
    let visit_typ_pos t = success t
  end)

let process = Walker_TypeCheck.walk_program