
open Common.VisitorMonad
open Common.AST
open Common.Meta
open Common.Context
open Common.Data

module Scope = Common.Scope

module Walker_LlvmPass = Common.Walker.Make(struct
    type ctx = context
    type err = pass_error
    type mta = meta

    let pop_val =
      get >>= function
      | `Codegen state -> begin
          match state.C.values with
          | [] -> error @@ CodegenError ValueStackEmpty
          | (vl :: rest) ->
            put @@ `Codegen { state with values = rest } >>= fun () ->
            success vl
        end
      | _ -> assert false
    let push_val vl =
      get >>= function
      | `Codegen state ->
        put @@ `Codegen { state with C.values = vl :: state.C.values }
      | _ -> assert false

    let pop_blk =
      get >>= function
      | `Codegen state -> begin
          match state.C.blocks with
          | [] -> error @@ CodegenError BlockStackEmpty
          | (vl :: rest) ->
            put @@ `Codegen { state with blocks = rest } >>= fun () ->
            print_string "POP ";
            print_endline @@ Llvm.value_name @@ Llvm.value_of_block vl;
            success vl
        end
      | _ -> assert false
    let push_blk bl =
      get >>= function
      | `Codegen state ->
        print_string "PUT ";
        print_endline @@ Llvm.value_name @@ Llvm.value_of_block bl;
        put @@ `Codegen { state with C.blocks = bl :: state.C.blocks }
      | _ -> assert false

    let pek_blk =
      get >>= function
      | `Codegen { C.blocks = bl :: other; _ } ->
        success bl
      | _ -> error @@ CodegenError BlockStackEmpty

    let print_stack =
      get >>= function
      | `Codegen state ->
        let stringify v =
          String.concat "|" [Llvm.string_of_llvalue v; Llvm.string_of_lltype @@ Llvm.type_of v] in
        let mapper v = print_endline @@ stringify v in
        let _ = List.map mapper state.C.values in
        success ()
      | _ -> assert false

    let con = Llvm.global_context ()
    let mdl = Llvm.create_module con "llpdc"
    let bdr = Llvm.builder con

    let rec typ_to_llvm = function
      | Array (_typ, _size, _) as t ->
        let rec resolve_array typ accum = begin match typ with
          | Array (inner, size, _) ->
            let (accum, leaf) = resolve_array inner accum in
            (size :: accum, leaf)
          | _ as leaf -> (accum, typ_to_llvm leaf)
        end in
        let (accum, leaf) = resolve_array t [] in
        List.fold_left Llvm.array_type leaf accum
      | Int _ -> Llvm.i64_type con
      | Float _ -> Llvm.double_type con
      | Char _ -> Llvm.i8_type con
      | Bool _ -> Llvm.i1_type con

    let visit_program_pre _ p =
      let main_type = Llvm.function_type (Llvm.i64_type con) [||] in
      let main = Llvm.declare_function "main" main_type mdl in
      let main_bb = Llvm.append_block con "entry" main in
      let () = Llvm.position_at_end main_bb bdr in
      push_blk main_bb >>= fun () ->
      success p
    let visit_program_pos _ p =
      (* ignore @@ Base.Option.map (Llvm_analysis.verify_module mdl) ~f:print_endline; *)
      Llvm.dump_module mdl;
      Llvm.dispose_module mdl;
      Llvm.dispose_context con;
      success p

    let scope_block_pre _ b =
      get >>= function
      | `Codegen state ->
        let new_state = { state with C.scopes = StringMap.empty :: state.C.scopes } in
        put @@ `Codegen new_state >>= fun _ ->
        success b
      | _ -> assert false
    let scope_block_pos _ b =
      get >>= function
      | `Codegen state ->
        let new_state = { state with C.scopes = List.tl state.C.scopes } in
        put @@ `Codegen new_state >>= fun _ ->
        success b
      | _ -> assert false
    let visit_block_pre _ b = success b
    let visit_block_pos _ b = success b

    (* Llvm. *)
    (* LLVM
       br
       if:
       then:
       endif:
        merge blk
    *)
    let visit_stmt_pre parent s =
      begin match parent with
        | `Stmt If _ ->
          let func = Llvm.block_parent @@ Llvm.insertion_block bdr in
          let block = Llvm.append_block con "cond" func in
          Llvm.position_at_end block bdr;
          push_blk block >>= fun _ ->
          success s
        | _ -> success s
      end
    let visit_stmt_pos parent s = begin match s with
      | Assign (loc, expr, _) as s ->
        pop_val >>= fun expr_llval ->
        pop_val >>= fun loc_llval ->
        let _ = Llvm.build_store expr_llval loc_llval bdr in
        success s
      | If (expr, stmt, stmt_opt, _) as s ->
        let func = Llvm.block_parent @@ Llvm.insertion_block bdr in
        pop_blk >>= fun cond_false ->
        pop_blk >>= fun cond_false_begin ->
        pop_blk >>= fun cond_true ->
        pop_blk >>= fun cond_true_begin ->
        pek_blk >>= fun previous ->
        pop_val >>= fun cond ->
        (* jump to previous block and create the branch instr *)
        Llvm.position_at_end previous bdr;
        ignore @@ Llvm.build_cond_br cond cond_true_begin cond_false_begin bdr;
        (* link to post block *)
        let post_block = Llvm.append_block con "ifend" func in
        (* branch true to end *)
        Llvm.position_at_end cond_true bdr;
        ignore @@ Llvm.build_br post_block bdr;
        (* branch false to end TODO if it exists *)
        Llvm.position_at_end cond_false bdr;
        ignore @@ Llvm.build_br post_block bdr;
        (* do the phi node stuff *)
        Llvm.position_at_end post_block bdr;
        success s
      | While (expr, stmt, _) as s -> success s
      | Do (expr, stmt, _) as s -> success s
      | Break _ as s -> success s
      | BlockStmt (block, _) as s -> success s
    end >>= fun s ->
      success parent >>= function
      | `Stmt If _ ->
        push_blk @@ Llvm.insertion_block bdr >>= fun () ->
        success s
      | _ -> success s

    let visit_decl_pre _ d = success d
    let visit_decl_pos _ = function
      | Decl (typ, ident, _) as d ->
        let lltyp = typ_to_llvm typ in
        get >>= begin function
          | `Codegen state ->
            begin match state.C.scopes with
              | curr :: others ->
                let llval = Llvm.build_alloca lltyp "tmpalloca" bdr in
                let new_scope = StringMap.add ident llval curr in
                let new_state = { state with C.scopes = new_scope :: others } in
                put @@ `Codegen new_state >>= fun _ ->
                success d
              | _ -> assert false
            end
          | _ -> assert false
        end

    let visit_expr_pre _ e = success e
    let visit_expr_pos _ = function
      (* Operations *)
      | BinOp (Typed(typ, _lhs, _), op, _rhs, _) as e ->
        pop_val >>= fun rhs_llval ->
        pop_val >>= fun lhs_llval ->
        let build f ff ident =
          begin match typ with
            | Float _ -> ff
            | _ -> f
          end lhs_llval rhs_llval ident bdr in
        push_val @@ begin match op with
          (* Arithmetic *)
          | Add _ ->
            build Llvm.build_add Llvm.build_fadd "addtmp"
          | Subtract _ ->
            build Llvm.build_sub Llvm.build_fsub "subtmp"
          | Multiply _ ->
            build Llvm.build_mul Llvm.build_fmul "multmp"
          | Divide _ ->
            build Llvm.build_sdiv Llvm.build_fdiv "divtmp"
          (* Boolean *)
          | Or _ ->
            Llvm.build_or lhs_llval rhs_llval "ortmp" bdr
          | And _ ->
            Llvm.build_and lhs_llval rhs_llval "andtmp" bdr
          (* Comparative *)
          | Eq _ ->
            let ieq = Llvm.build_icmp Llvm.Icmp.Eq in
            let feq = Llvm.build_fcmp Llvm.Fcmp.Oeq in
            build ieq feq "eqtmp"
          | Neq _ ->
            let ineq = Llvm.build_icmp Llvm.Icmp.Ne in
            let fneq = Llvm.build_fcmp Llvm.Fcmp.One in
            build ineq fneq "neqtmp"
          | Lt _ ->
            let ilt = Llvm.build_icmp Llvm.Icmp.Slt in
            let flt = Llvm.build_fcmp Llvm.Fcmp.Olt in
            build ilt flt "lttmp"
          | Leq _ ->
            let ileq = Llvm.build_icmp Llvm.Icmp.Sle in
            let fleq = Llvm.build_fcmp Llvm.Fcmp.Ole in
            build ileq fleq "leqtmp"
          | Geq _ ->
            let igeq = Llvm.build_icmp Llvm.Icmp.Sge in
            let fgeq = Llvm.build_fcmp Llvm.Fcmp.Oge in
            build igeq fgeq "geqtmp"
          | Gt _ ->
            let igt = Llvm.build_icmp Llvm.Icmp.Sgt in
            let fgt = Llvm.build_fcmp Llvm.Fcmp.Ogt in
            build igt fgt "gttmp"
        end >>= fun _ ->
        success e
      | UnOp (op, Typed (typ, _expr, _), _) as e ->
        pop_val >>= fun llval ->
        begin match op with
          | Negate _ -> begin match typ with
              | Int _ -> push_val @@ Llvm.build_neg llval "tmpineg" bdr
              | Float _ -> push_val @@ Llvm.build_fneg llval "tmpfneg" bdr
              | _ -> error @@ CodegenError (NegateOnlyIntOrFloat e)
            end
          | Not _ -> (* bool *)
            push_val @@ Llvm.build_not llval "tmpnot" bdr
        end >>= fun _ ->
        success e
      (* Constants *)
      | Const (Num (i, _), _) as e ->
        push_val @@ Llvm.const_int (Llvm.i64_type con) i >>= fun () ->
        success e
      | Const (Real (r, _), _) as e ->
        push_val @@ Llvm.const_float (Llvm.double_type con) r >>= fun () ->
        success e
      | Const (Bool (b, _), _) as e ->
        push_val @@ Llvm.const_int (Llvm.i1_type con) (if b then 1 else 0) >>= fun () ->
        success e
      (* Variables *)
      | Var (loc, _) as e ->
        pop_val >>= fun llval_ptr ->
        push_val @@ Llvm.build_load llval_ptr "tmpload" bdr >>= fun _ ->
        success e
      | Typed (typ, expr, _) as e -> success e
      | _ as e -> error @@ CodegenError (CannotGenerateExpression e)

    let visit_loc_pre _ l = success l
    let visit_loc_pos _ = function
      (* Identifiers *)
      | Id (ident, m) as l ->
        Scope.get_llvalue m ident >>= fun llval ->
        push_val llval >>= fun _ ->
        success l
      (* Array Dereferences (enforced through tcp) *)
      | Deref (loc, expr, _) as l ->
        pop_val >>= fun expr_llval ->
        pop_val >>= fun loc_llval ->
        let indices = [|
          Llvm.const_int (Llvm.i64_type con) 0;
          expr_llval
        |] in
        push_val @@ Llvm.build_gep loc_llval indices "tmpgep" bdr >>= fun _ ->
        success l
      | LTyped (typ, loc, _) as l -> success l
    (* | _ as l -> error @@ CodegenError (CannotGenerateLocation l) *)

    let visit_typ_pre _ t = success t
    let visit_typ_pos _ t = success t
  end)
let process = Walker_LlvmPass.walk_program `Root