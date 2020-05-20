open Common.VisitorMonad
open Common.AST
open Common.Meta
open Common.Context

module Scope = Common.Scope

(* While the TypeCheckPass lets type information bubble up,
   this pass pushes it down where placeholders are left.
   Currently the only placeholder-capable type is Int,
   which denotes its placeholder precision value with the
   sentinel -1 value. *)
module Walker_TypeInferPass = Common.Walker.Make(struct
    type ctx = context
    type err = pass_error
    type mta = meta

    let visit_program_pre _ p = success p
    let visit_program_pos _ p = success p

    let scope_block_pre _ b = success b
    let scope_block_pos _ b = success b
    let visit_block_pre _ b = success b
    let visit_block_pos _ b = success b

    let visit_stmt_pre _ = function
      | Assign (LTyped (Int (prec, _), _, _) as lt, Typed (Int (-1, tm), expr, em), sm) ->
        success @@
        Assign (lt, Typed (Int (prec, tm), expr, em), sm)
      | _ as s -> success s
    (* success s *)
    let visit_stmt_pos _ s = success s

    let visit_decl_pre _ d = success d
    let visit_decl_pos _ d = success d

    let visit_expr_pre _ =
      function
      (* Push parent Int precision into children *)
      | Typed (Int (prec, _) as pt, expr, m) ->
        success @@
        Typed (pt,
               begin match expr with
                 | BinOp (Typed (Int (-1, ilm), lex, lm), op, Typed (Int (-1, irm), rex, rm), em) ->
                   BinOp (Typed (Int (prec, ilm), lex, lm), op, Typed (Int (prec, irm), rex, rm), em)
                 | BinOp (Typed (Int (lprec, ilm), lex, lm), op, Typed (Int (-1, irm), rex, rm), em) ->
                   BinOp (Typed (Int (lprec, ilm), lex, lm), op, Typed (Int (prec, irm), rex, rm), em)
                 | BinOp (Typed (Int (-1, ilm), lex, lm), op, Typed (Int (rprec, irm), rex, rm), em) ->
                   BinOp (Typed (Int (prec, ilm), lex, lm), op, Typed (Int (rprec, irm), rex, rm), em)
                 | UnOp (op, Typed (Int (-1, im), lex, lm), em) ->
                   UnOp (op, Typed (Int (prec, im), lex, lm), em)
                 | Const (Num (lv, -1, lm), em) ->
                   Const (Num (lv, prec, lm), em)
                 | _ as e -> e
               end, m)
      (* Push sibling Int precision into children, or set default 64 *)
      | Typed ((Bool _) as pt, expr, m) ->
        success @@
        Typed (pt, begin match expr with
            | BinOp (Typed (Int (-1, ilm), lex, lm), op, Typed (Int (-1, irm), rex, rm), em) ->
              BinOp (Typed (Int (64, ilm), lex, lm), op, Typed (Int (64, irm), rex, rm), em)
            | BinOp (Typed (Int (lprec, ilm), lex, lm), op, Typed (Int (-1, irm), rex, rm), em) ->
              BinOp (Typed (Int (lprec, ilm), lex, lm), op, Typed (Int (lprec, irm), rex, rm), em)
            | BinOp (Typed (Int (-1, ilm), lex, lm), op, Typed (Int (rprec, irm), rex, rm), em) ->
              BinOp (Typed (Int (rprec, ilm), lex, lm), op, Typed (Int (rprec, irm), rex, rm), em)
            | _ as e -> e
          end, m)
      | _ as e -> success e
    let visit_expr_pos _ e = success e

    let visit_loc_pre _ l = success l
    let visit_loc_pos _ l = success l

    let visit_typ_pre _ t = success t
    let visit_typ_pos _ t = success t
  end)

let process = Walker_TypeInferPass.walk_program `Root