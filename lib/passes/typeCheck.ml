open Common.VisitorMonad
open PassContext
open Common.AST
open Common.Meta

let unop_compatible = function
  | (Negate _, Int _) | (Negate _, Float _) -> true
  | (Not _, Bool _) -> true
  | _ -> false

let wrap_type typ e m = success @@ Typed (typ, e, m)

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

let rec same_typ = function
  | (Array (left_typ, left_size, _), Array (right_typ, right_size, _)) 
    -> same_typ (left_typ, right_typ) && left_size == right_size
  | (Int _, Int _)
  | (Float _, Float _)
  | (Char _, Char _) 
  | (Bool _, Char _) -> true
  | _ -> false

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
        if (same_typ (ltyp, typ)) then success s
        else error @@ TypeError (IncompatibleAssignment (ltyp, typ))
      | If (Typed (typ, _, _), _, _, _) as s -> 
        (match typ with 
         |Bool _ -> success s
         | _ -> error @@ TypeError IfRequiresBoolean)
      | While (Typed (typ, _, _), _, _) as s ->
        (match typ with 
         |Bool _ -> success s
         | _ -> error @@ TypeError WhileRequiresBoolean)
      | Do (Typed (typ, _, _), _, _) as s ->
        (match typ with 
         |Bool _ -> success s
         | _ -> error @@ TypeError DoRequiresBoolean)
      | Break _ as s -> success s
      | BlockStmt _ as s -> success s
      | _ as s -> error @@ TypeError (UntypedStatementFragment s)

    let visit_decl_pre d = success d
    let visit_decl_pos d = success d

    let visit_expr_pre e = success e
    let visit_expr_pos = function
      | BinOp (Typed (lt, _, _), op, Typed (rt, _, _), m) as e -> (
          match binop_result (lt, op, rt, m) with
          | Some merged_type -> wrap_type merged_type e m
          | None -> error @@ TypeError (IncompatibleBinOp (lt, op, rt)))
      | UnOp (op, Typed (typ, _, _), m) as e ->
        if unop_compatible (op, typ)
        then wrap_type typ e m
        else error @@ TypeError (IncompatibleUnOp (op, typ))
      | Const (Num (_, nm), m) as v -> success @@ Typed (Int nm, v, m)
      | Const (Real (_, rm), m) as v -> success @@ Typed (Float rm, v, m)
      | Const (Bool (_, bm), m) as v -> success @@ Typed (Bool bm, v, m)
      | Var (LTyped (typ, loc, tm), vm) -> success @@ Typed (typ, (Var (loc, vm)), tm)
      | Typed _ as typed -> success typed
      | _ as e -> error @@ TypeError (UntypedSubExpressions e)

    let visit_loc_pre l = success l
    let visit_loc_pos = function
      | Deref (LTyped (Array (atyp, _, _) as arr, loc, lm), expr, m) -> 
        success @@ LTyped (atyp, Deref (LTyped (arr, loc, lm), expr, m), m)
      | Id (ident, m) as l -> 
        Scope.get_type ident >>= fun ident_typ ->
        success @@ LTyped (ident_typ, l, m)
      | LTyped _ as typed -> success typed
      | _ as l  -> error @@ TypeError (UntypedSubLocations l)

    let visit_typ_pre t = success t
    let visit_typ_pos t = success t
  end)

let process = Walker_TypeCheck.walk_program