open Common.VisitorMonad
open Common.AST
open Common.Meta
open Common.Context

module Scope = Common.Scope

let unop_compatible = function
  | (Negate _, Int _) | (Negate _, Float _) -> true
  | (Not _, Bool _) -> true
  | _ -> false

let wrap_type typ e m = success @@ Typed (typ, e, m)

let rec same_typ = function
  | (Array (left_typ, left_size, _), Array (right_typ, right_size, _))
    -> same_typ (left_typ, right_typ) && left_size == right_size
  | (Int _, Int _)
  | (Float _, Float _)
  | (Char _, Char _)
  | (Bool _, Bool _) -> true
  | _ -> false

let binop_result = function
  (* Equality *)
  | (t, Eq _, t', m) | (t, Neq _, t', m) when (same_typ (t, t')) -> Some (Bool m)
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
  | (Int _, Remainder _, Int _, m) -> Some (Int m)
  (* (Float) Arithmetic *)
  | (Float _, Add _, Float _, m)
  | (Float _, Subtract _, Float _, m)
  | (Float _, Multiply _, Float _, m)
  | (Float _, Divide _, Float _, m) -> Some (Float m)
  | (Float _, Remainder _, Float _, m) -> Some (Float m)
  | _ -> None

module Walker_TypeCheckPass = Common.Walker.Make(struct
    type ctx = context
    type err = pass_error
    type mta = meta

    let visit_program_pre _ p = success p
    let visit_program_pos _ p = success p

    let scope_block_pre = Scope.scope_block_pre
    let scope_block_pos = Scope.scope_block_pos
    let visit_block_pre _ b = success b
    let visit_block_pos _ b = success b

    let visit_stmt_pre _ s = success s
    let visit_stmt_pos _ = function
      | Assign (LTyped(ltyp, _, _), Typed(typ, _, _), _) as s ->
        if (same_typ (ltyp, typ)) then success s
        else error @@ TypeError (IncompatibleAssignment (s, ltyp, typ))
      | ProbAssign (LTyped(ltyp, _, _), exprs, _) as s ->
        let rec types_match_ltyp = begin function
          | Typed(typ, _, _) :: rest ->
            if (same_typ (ltyp, typ)) then types_match_ltyp rest
            else error @@ TypeError (IncompatibleAssignment (s, ltyp, typ))
          | [] -> success ()
          | _ -> error @@ TypeError (UntypedStatementFragment s)
        end in
        types_match_ltyp exprs >>= fun () ->
        success s
      | If (Typed (typ, _, _), _, _, _) as s -> begin
          match typ with
          | Bool _ -> success s
          | _ -> error @@ TypeError (IfRequiresBoolean s)
        end
      | While (Typed (typ, _, _), _, _) as s -> begin
          match typ with
          | Bool _ -> success s
          | _ -> error @@ TypeError (WhileRequiresBoolean s)
        end
      | Do (Typed (typ, _, _), _, _) as s -> begin
          match typ with
          | Bool _ -> success s
          | _ -> error @@ TypeError (DoRequiresBoolean s)
        end
      | Break _ as s -> success s
      | Print _ as s -> success s
      | Choose _ as s -> success s
      | BlockStmt _ as s -> success s
      | _ as s -> error @@ TypeError (UntypedStatementFragment s)

    let visit_decl_pre _ d = success d
    let visit_decl_pos _ d = success d

    let visit_expr_pre _ e = success e
    let visit_expr_pos _ = function
      | BinOp (Typed (lt, _, _), op, Typed (rt, _, _), m) as e -> (
          match binop_result (lt, op, rt, m) with
          | Some merged_type -> wrap_type merged_type e m
          | None -> error @@ TypeError (IncompatibleBinOp (e, lt, op, rt)))
      | UnOp (op, Typed (typ, _, _), m) as e ->
        if unop_compatible (op, typ)
        then wrap_type typ e m
        else error @@ TypeError (IncompatibleUnOp (e, op, typ))
      | Const (Num (_, nm), m) as v -> success @@ Typed (Int nm, v, m)
      | Const (Real (_, rm), m) as v -> success @@ Typed (Float rm, v, m)
      | Const (Bool (_, bm), m) as v -> success @@ Typed (Bool bm, v, m)
      | Var (LTyped (typ, loc, tm), vm) -> success @@ Typed (typ, (Var (loc, vm)), tm)
      | Typed _ as typed -> success typed
      | _ as e -> error @@ TypeError (UntypedSubExpressions e)

    let visit_loc_pre _ l = success l
    let visit_loc_pos _ = function
      | Deref (LTyped (Array (atyp, _, _) as arr, loc, lm), expr, m) ->
        success @@ LTyped (atyp, Deref (LTyped (arr, loc, lm), expr, m), m)
      | Id (ident, m) as l ->
        Scope.get_typ m ident >>= fun ident_typ ->
        success @@ LTyped (ident_typ, l, m)
      | LTyped _ as typed -> success typed
      | _ as l  -> error @@ TypeError (UntypedSubLocations l)

    let visit_typ_pre _ t = success t
    let visit_typ_pos _ t = success t
  end)

let process = Walker_TypeCheckPass.walk_program `Root