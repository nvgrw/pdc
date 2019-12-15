open Common.VisitorMonad
open PassContext
open Common.AST

(* 
  Strategies for resolving types:

  Is operator legal?
  Apply type to all super-expressions.
  Do subtyping/further checks and propagate errors if there are any
 *)

(* 
type typ =
  | Array of typ * int
  | Int
  | Float
  | Char
  | Bool
 *)

(* Trivial combination is equality *)
(* let combine a b = a == b *)

let unop_compatible = function
  | Negate, Int | Negate, Float -> true
  | Not, Bool -> true
  | _ -> false

let binop_result = function
  (* Equality *)
  | t, Eq, t' | t, Neq, t' when t == t' -> Some Bool
  (* (Int) Inequality *)
  | Int, Lt, Int | Int, Leq, Int | Int, Geq, Int | Int, Gt, Int -> Some Bool
  (* (Float) Inequality *)
  | Float, Lt, Float | Float, Leq, Float | Float, Geq, Float | Float, Gt, Float -> Some Bool
  (* (Bool) Arithmetic *)
  | Bool, Or, Bool | Bool, And, Bool -> Some Bool
  (* (Int) Arithmetic *)
  | Int, Add, Int | Int, Subtract, Int | Int, Multiply, Int | Int, Divide, Int -> Some Int
  (* (Float) Arithmetic *)
  | Float, Add, Float | Float, Subtract, Float | Float, Multiply, Float | Float, Divide, Float -> Some Float
  | _ -> None

module Walker_TypeCheck = Common.Walker.Make(struct 
    type ctx = context
    type err = pass_error

    let visit_program_pre p = success p
    let visit_program_pos p = success p

    let visit_block_pre b = success b
    let visit_block_pos b = success b

    let visit_stmt_pre s = success s
    let visit_stmt_pos s = success s

    let visit_decl_pre d = success d
    let visit_decl_pos d = success d

    let wrap_type typ e = success @@ Typed (typ, e)

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
      (* | Var loc as e -> success e *)
      (* | Typed (typ, expr) as e -> success e *)
      | _ as e -> success e

    let visit_loc_pre l = success l
    let visit_loc_pos l = success l

    let visit_typ_pre t = success t
    let visit_typ_pos t = success t
  end)

let process = Walker_TypeCheck.walk_program