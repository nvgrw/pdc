
open Common.VisitorMonad
open Common.AST
open Common.Meta
open Common.Context
open Common.Data

module Scope = Common.Scope
module Option = Base.Option

let mdl_ref = ref None
let initialize mdl = mdl_ref := Some mdl

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
            success vl
        end
      | _ -> assert false
    let push_blk bl =
      get >>= function
      | `Codegen state ->
        put @@ `Codegen { state with C.blocks = bl :: state.C.blocks }
      | _ -> assert false

    let print_val_stack =
      get >>= function
      | `Codegen state ->
        let stringify v =
          String.concat "|" [Llvm.string_of_llvalue v; v |> Llvm.type_of |> Llvm.string_of_lltype] in
        let mapper v = print_endline @@ stringify v in
        let _ = List.map mapper state.C.values in
        success ()
      | _ -> assert false

    let print_blk_stack =
      get >>= function
      | `Codegen { C.blocks = blocks; _ } ->
        let block_ident b = b |> Llvm.value_of_block |> Llvm.value_name in
        print_endline "v Block Stack Dump";
        ignore @@ List.map (fun b -> print_endline @@ block_ident b) blocks;
        print_endline "^ Block Stack Dump";
        success ()
      | _ -> assert false

    let con = Llvm.global_context ()
    let bdr = Llvm.builder con

    let push_break_block func =
      get >>= function
      | `Codegen ({ C.breakBlocks = breakBlocks; _ } as state) ->
        put @@ `Codegen {
          state with C.breakBlocks =
                       (Llvm.append_block con "break" func) :: breakBlocks
        }
      | _ -> assert false

    let configure_break_block end_block =
      get >>= begin function
        | `Codegen ({ C.breakBlocks = bb :: rest; _ } as state) ->
          put @@ `Codegen { state with C.breakBlocks = rest } >>= fun () ->
          success bb
        | _ -> assert false end
      >>= fun break_block ->
      Llvm.move_block_before end_block break_block;
      begin match Llvm.use_begin (Llvm.value_of_block break_block) with
        | None ->
          (* remove redundant break block *)
          Llvm.delete_block break_block
        | Some _ ->
          (* branch break block to end of construct *)
          Llvm.position_at_end break_block bdr;
          ignore @@ Llvm.build_br end_block bdr
      end;
      success ()

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

    let build_br_if_not_terminated bb bdr =
      match bdr |> Llvm.insertion_block |> Llvm.block_terminator with
      | None ->
        Some (Llvm.build_br bb bdr)
      | Some _ -> None
    let build_cond_br_if_not_terminated cond cond_true cond_false bdr =
      match bdr |> Llvm.insertion_block |> Llvm.block_terminator with
      | None ->
        Some (Llvm.build_cond_br cond cond_true cond_false bdr)
      | Some _ -> None

    let visit_program_pre _ p =
      let mdl = match !mdl_ref with | Some mdl -> mdl | None -> assert false in
      let main_type = Llvm.function_type (Llvm.i64_type con) [||] in
      let main = Llvm.declare_function "main" main_type mdl in
      let main_bb = Llvm.append_block con "entry" main in
      let () = Llvm.position_at_end main_bb bdr in
      success p
    let visit_program_pos _ p =
      ignore @@ Llvm.build_ret (Llvm.const_int (Llvm.i64_type con) 0) bdr;

      let mdl = match !mdl_ref with | Some mdl -> mdl | None -> assert false in
      mdl_ref := None;

      (* Verify *)
      begin match Llvm_analysis.verify_module mdl with
        | Some message ->
          error @@ CodegenError (ModuleVerification message)
        | None -> success ()
      end >>= fun () ->
      (* End of Verification *)

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

    let visit_stmt_pre parent s =
      let func = bdr |> Llvm.insertion_block |> Llvm.block_parent in
      begin match parent with
        | `Stmt If _ ->
          let block = Llvm.append_block con "cond" func in
          Llvm.position_at_end block bdr;
          push_blk block >>= fun () ->
          success s
        | `Stmt While _ ->
          let block = Llvm.append_block con "wbody" func in
          Llvm.position_at_end block bdr;
          push_blk block >>= fun () ->
          success s
        | `Stmt Do _ ->
          let block = Llvm.append_block con "dbody" func in
          Llvm.position_at_end block bdr;
          push_blk block >>= fun () ->
          success s
        | `Stmt Choose _ ->
          (* this is necessary as walking is deferred but non-pure Llvm append modifies state *)
          success () >>= fun () ->
          let block = Llvm.append_block con "prob" func in
          Llvm.position_at_end block bdr;
          push_blk block >>= fun () ->
          success s
        | _ -> success s
      end >>= function
      | If _ ->
        push_blk @@ Llvm.insertion_block bdr >>= fun () ->
        success s
      | While _ ->
        let block = Llvm.append_block con "whead" func in
        ignore @@ Llvm.build_br block bdr;
        push_blk block >>= fun () ->
        (* add break block *)
        push_break_block func >>= fun () ->
        success s
      | Do _ ->
        push_blk @@ Llvm.insertion_block bdr >>= fun () ->
        (* add break block *)
        push_break_block func >>= fun () ->
        success s
      | Choose _ ->
        push_blk @@ Llvm.insertion_block bdr >>= fun () ->
        success s
      | _ ->
        success s
    let visit_stmt_pos parent s =
      let func = bdr |> Llvm.insertion_block |> Llvm.block_parent in
      begin match s with
        | Assign (loc, expr, _) as s ->
          pop_val >>= fun expr_llval ->
          pop_val >>= fun loc_llval ->
          let _ = Llvm.build_store expr_llval loc_llval bdr in
          success s
        | ProbAssign _ as s ->
          error @@ CodegenError (ProbAssignNotLowered s)
        | If (expr, stmt, Some stmt_opt, _) as s ->
          pop_blk >>= fun cond_false ->
          pop_blk >>= fun cond_false_begin ->
          pop_blk >>= fun cond_true ->
          pop_blk >>= fun cond_true_begin ->
          pop_blk >>= fun previous ->
          pop_val >>= fun cond ->
          (* jump to previous block and create the branch instr *)
          Llvm.position_at_end previous bdr;
          ignore @@ Llvm.build_cond_br cond cond_true_begin cond_false_begin bdr;
          (* link to post block *)
          let post_block = Llvm.append_block con "ifend" func in
          (* branch true to end *)
          Llvm.position_at_end cond_true bdr;
          ignore @@ build_br_if_not_terminated post_block bdr;
          (* branch false to end *)
          Llvm.position_at_end cond_false bdr;
          ignore @@ build_br_if_not_terminated post_block bdr;
          (* do the phi node stuff *)
          Llvm.position_at_end post_block bdr;
          success s
        | If (expr, stmt, None, _) as s ->
          pop_blk >>= fun cond_true ->
          pop_blk >>= fun cond_true_begin ->
          pop_blk >>= fun previous ->
          pop_val >>= fun cond ->
          (* jump to previous block and create the branch instr *)
          Llvm.position_at_end previous bdr;
          (* link to post block *)
          let post_block = Llvm.append_block con "ifend" func in
          ignore @@ Llvm.build_cond_br cond cond_true_begin post_block bdr;
          (* branch true to end *)
          Llvm.position_at_end cond_true bdr;
          ignore @@ build_br_if_not_terminated post_block bdr;
          (* do the phi node stuff *)
          Llvm.position_at_end post_block bdr;
          success s
        | While (expr, stmt, _) as s ->
          pop_blk >>= fun body ->
          pop_blk >>= fun head ->
          pop_val >>= fun cond ->
          ignore @@ build_br_if_not_terminated head bdr;
          let wend = Llvm.append_block con "wend" func in
          Llvm.position_at_end head bdr;
          ignore @@ Llvm.build_cond_br cond body wend bdr;
          (* move break block *)
          configure_break_block wend >>= fun () ->
          (* move to end *)
          Llvm.position_at_end wend bdr;
          success s
        | Do (expr, stmt, _) as s ->
          pop_blk >>= fun body ->
          pop_blk >>= fun previous ->
          pop_val >>= fun cond ->
          let current_block = Llvm.insertion_block bdr in
          Llvm.position_at_end previous bdr;
          ignore @@ Llvm.build_br body bdr;
          Llvm.position_at_end current_block bdr;
          let dend = Llvm.append_block con "dend" func in
          ignore @@ build_cond_br_if_not_terminated cond body dend bdr;
          (* move break block *)
          configure_break_block dend >>= fun () ->
          (* move to end *)
          Llvm.position_at_end dend bdr;
          success s
        | Break _ as s ->
          get >>= begin function
            | `Codegen { C.breakBlocks = bb :: _; _ } ->
              success bb
            | _ -> assert false
          end >>= fun break_block ->
          ignore @@ Llvm.build_br break_block bdr;
          success s
        | Print (Typed (typ, _, _), _) as s ->
          pop_val >>= fun vl ->
          let mdl = match !mdl_ref with | Some mdl -> mdl | None -> assert false in
          let printf_func = match Llvm.lookup_function "printf" mdl with | Some mdl -> mdl | None -> assert false in
          let format = Llvm.build_global_stringptr begin match typ with
              | Array _ -> "%.0snot implemented\n"
              | Int _ | Char _ | Bool _ -> "%d\n"
              | Float _ -> "%f\n"
            end "" bdr in
          ignore @@ Llvm.build_call printf_func [| format; vl |] "" bdr;
          success s
        | Choose (stmts, probs, _) ->
          seqList @@ List.map (fun _ ->
              pop_blk >>= fun prob_end ->
              pop_blk >>= fun prob_begin ->
              success (prob_begin, prob_end)) probs >>= fun prob_blocks ->
          pop_blk >>= fun previous ->
          let (prob_begins, prob_ends) = List.rev prob_blocks |> List.split in
          let mdl = match !mdl_ref with | Some mdl -> mdl | None -> assert false in

          (* jump to previous block and create jump table *)
          Llvm.position_at_end previous bdr;
          let random_func = match Llvm.lookup_function "random" mdl with | Some mdl -> mdl | None -> assert false in
          let random = Llvm.build_call random_func [||] "rnd" bdr in
          let prob_sum = List.fold_left (+) 0 probs in
          let prob_sum_const = Llvm.const_float (Llvm.double_type con) (float_of_int prob_sum) in
          let choice_float = Llvm.build_fmul random prob_sum_const "choicef" bdr in
          let choice = Llvm.build_fptoui choice_float (Llvm.i64_type con) "choice" bdr in

          let switch = Llvm.build_switch choice (List.hd prob_begins) prob_sum bdr in
          (* add cases only for the other probs *)
          let foreach_prob total (p, blk) = begin
            for i = total to total + p - 1 do
              Llvm.add_case switch (Llvm.const_int (Llvm.i64_type con) i) blk
            done;
            total + p
          end in
          let _ = List.fold_left foreach_prob (List.hd probs) (List.tl (List.combine probs prob_begins)) in

          (* insert unconditional branch to post_block *)
          let post_block = Llvm.append_block con "chooseend" func in
          List.iter (fun b ->
              Llvm.position_at_end b bdr;
              ignore @@ build_br_if_not_terminated post_block bdr) prob_ends;

          Llvm.position_at_end post_block bdr;
          success s
        | BlockStmt (block, _) as s -> success s
        | _ as s ->
          error @@ CodegenError (UnimplementedStatement s)
      end >>= fun s ->
      success parent >>= function
      | `Stmt If _ ->
        push_blk @@ Llvm.insertion_block bdr >>= fun () ->
        success s
      | `Stmt Choose _ ->
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