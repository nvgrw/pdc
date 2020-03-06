
open Common.VisitorMonad
open Common.AST
open Common.Meta

open Context

module Walker_LlvmPass = Common.Walker.Make(struct 
    type ctx = context
    type err = pass_error
    type mta = meta

    let pop_val =
      get >>= fun state -> 
      match state.values with
      | [] -> error @@ Message "popped empty values list"
      | vl :: rest ->
        put { values = rest } >>= fun () ->
        success vl

    let push_val vl =
      get >>= fun state ->
      put { values = vl :: state.values }

    let con = Llvm.global_context ()
    let mdl = Llvm.create_module con "llpdc"
    let bdr = Llvm.builder con

    let int_type = Llvm.i64_type con
    let real_type = Llvm.double_type con

    let bool_type = Llvm.i1_type con

    let visit_program_pre p = success p
    let visit_program_pos p = success p

    let scope_block_pre b = success b
    let scope_block_pos b = success b
    let visit_block_pre b = success b
    let visit_block_pos b = success b

    let visit_stmt_pre s = success s
    let visit_stmt_pos s = success s

    let visit_decl_pre d = success d
    let visit_decl_pos d = success d

    let visit_expr_pre e = success e
    let visit_expr_pos = function

      | BinOp (lhs, op, rhs, _) as e -> success e
      | UnOp (op, expr, _) as e -> success e
      | Const (Num (i, _), _) as e -> 
        push_val (Llvm.const_int int_type i) >>= fun () ->
        success e
      | Const (Real (r, _), _) as e -> 
        push_val (Llvm.const_float real_type r) >>= fun () ->
        success e
      | Const (Bool (b, _), _) as e -> 
        push_val (Llvm.const_int bool_type (if b then 1 else 0)) >>= fun () ->
        success e
      | Var (loc, _) as e -> success e
      (* maintain a mapping from var to llvm variables. they must be defined already *)
      | Typed (typ, expr, _) as e -> success e
    (* keep track of type information to generate the right code? *)

    let visit_loc_pre l = success l
    let visit_loc_pos l = success l

    let visit_typ_pre t = success t
    let visit_typ_pos t = success t
  end)
let process = Walker_LlvmPass.walk_program