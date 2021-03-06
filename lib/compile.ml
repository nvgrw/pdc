open Common.AST
open Common.VisitorMonad
open Common.Meta

open Setup

open Lexing
open Printf

let generate_context get_lines s_pos e_pos =
  let num_digits = e_pos.pos_lnum |> float_of_int  |> log10 |> int_of_float |> (+) 1 in
  let num_lines = e_pos.pos_lnum - s_pos.pos_lnum + 1 in
  let s_off = s_pos.pos_cnum - s_pos.pos_bol in
  let e_off = e_pos.pos_cnum - e_pos.pos_bol in
  let pointer i line =
    let padding = String.make num_digits ' ' ^ " : " in
    (* three cases: num_lines is 1, first or last line *)
    if num_lines == 1 then
      if s_off == e_off || e_off - s_off == 1 then
        (* single pointer *)
        [padding ^ String.make s_off ' ' ^ "^"]
      else
        (* multiple pointers *)
        [padding ^ String.make s_off ' ' ^ "^" ^ String.make (e_off - s_off - 2) '-' ^ "^"]
    else if i == 0 then
      [padding ^ String.make s_off ' ' ^ "^" ^ String.make (String.length line - s_off - 1) '-']
    else if i == num_lines - 1 then
      [padding ^ String.make (e_off - 1) '-' ^ "^"]
    else
      []
  in
  let each i line = sprintf "%0*d | %s" num_digits (s_pos.pos_lnum + i) line :: pointer i line in
  let code = List.mapi each @@ get_lines (s_pos.pos_lnum - 1) (e_pos.pos_lnum - 1) |> List.concat |> String.concat "\n" in
  let fname = match s_pos.pos_fname with | "" -> "<no file>" | _ -> s_pos.pos_fname in
  sprintf "%s\n%s:%d:%d;%d:%d" code fname s_pos.pos_lnum (s_off + 1)  e_pos.pos_lnum (e_off + 1)

let error_string err get_lines =
  let type_str = match err with
    | TypeError _ -> "type"
    | StructuralError _ -> "structural"
    | CodegenError _ -> "codegen"
    | Message _ -> "misc"
  in match err with
  | TypeError (IncompatibleBinOp (expr, lt, op, rt)) ->
    let context = (match get_meta_expr expr with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
    sprintf "%s: [%s] %s incompatible with types %s and %s." context type_str (show_binop pp_meta op) (show_typ pp_meta lt) (show_typ pp_meta rt)
  | TypeError (IncompatibleUnOp (expr, op, t)) ->
    let context = (match get_meta_expr expr with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
    sprintf "%s: [%s] %s incompatible with type %s." context type_str (show_unop pp_meta op) (show_typ pp_meta t)
  | TypeError (IncompatibleAssignment (stmt, ltyp, typ)) ->
    let context = (match get_meta_stmt stmt with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
    sprintf "%s: [%s] incompatible assignment between types %s and %s." context type_str (show_typ pp_meta ltyp) (show_typ pp_meta typ)
  | TypeError (IfRequiresBoolean stmt)->
    let context = (match get_meta_stmt stmt with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
    sprintf "%s: [%s] if statement requires condition to evaluate to boolean expression." context type_str
  | TypeError (WhileRequiresBoolean stmt) ->
    let context = (match get_meta_stmt stmt with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
    sprintf "%s: [%s] while statement requires condition to evaluate to boolean expression." context type_str
  | TypeError (DoRequiresBoolean stmt) ->
    let context = (match get_meta_stmt stmt with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
    sprintf "%s: [%s] do statement requires condition to evaluate to a boolean expression." context type_str
  | TypeError (UntypedSubExpressions expr) ->
    let context = (match get_meta_expr expr with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
    sprintf "%s: [%s] untyped subexpressions in expression %s." context type_str (show_expr pp_meta expr)
  | TypeError (UntypedSubLocations loc) ->
    let context = (match get_meta_loc loc with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
    sprintf "%s: [%s] untyped sublocations in location %s." context type_str (show_loc pp_meta loc)
  | TypeError (UntypedStatementFragment stmt) ->
    let context = (match get_meta_stmt stmt with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
    sprintf "%s: [%s] untyped statement fragment %s." context type_str (show_stmt pp_meta stmt)
  | TypeError (BadIntPrecision (t, precision)) ->
    let context = (match get_meta_typ t with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
    sprintf "%s: [%s] invalid int precision %d." context type_str precision
  | StructuralError (BadIdentifier (m, ident)) ->
    let context = (match m with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
    sprintf "%s: [%s] identifier `%s' not declared in scope." context type_str ident
  | StructuralError (DuplicateIdentifier (decl, ident)) ->
    let context = (match get_meta_decl decl with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
    sprintf "%s: [%s] identifier `%s' already declared in scope." context type_str ident
  | StructuralError (ChooseInvalidWeight (stmt, prob)) ->
    let context = (match get_meta_stmt stmt with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
    sprintf "%s: [%s] choice statement in choose statement has invalid weight %d." context type_str prob
  | CodegenError ValueStackEmpty ->
    sprintf "[%s] value stack empty" type_str
  | CodegenError BlockStackEmpty ->
    sprintf "[%s] block stack empty" type_str
  | CodegenError (CannotGenerateExpression expr) ->
    let context = (match get_meta_expr expr with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
    sprintf "%s: [%s] cannot generate expression %s." context type_str (show_expr pp_meta expr)
  | CodegenError (NegateOnlyIntOrFloat expr) ->
    let context = (match get_meta_expr expr with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
    sprintf "%s: [%s] can only negate int or float %s." context type_str (show_expr pp_meta expr)
  | CodegenError (CannotGenerateLocation loc) ->
    let context = (match get_meta_loc loc with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
    sprintf "%s: [%s] cannot generate expression %s." context type_str (show_loc pp_meta loc)
  | CodegenError (ModuleVerification msg) ->
    sprintf "[%s] module verification error: %s" type_str msg
  | CodegenError (ProbAssignNotLowered stmt) ->
    let context = (match get_meta_stmt stmt with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
    sprintf "%s: [%s] probabilistic assignment not lowered to choose before code generation." context type_str
  | CodegenError (UnimplementedStatement stmt) ->
    let context = (match get_meta_stmt stmt with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
    sprintf "%s: [%s] statement not implemented." context type_str
  | Message m -> sprintf "[%s] %s" type_str m

let generate (config: compile_conf) (p: meta program) (get_lines: int -> int -> string list) =
  if config.dump_lex_ast then config.printer @@ show_program pp_meta p;
  let post_semant = Semant.check p
  in begin match post_semant with
    | Error err ->
      config.printer @@ error_string err get_lines;
      None
    | Success (_, semant_ast) ->
      if config.dump_semant_ast then config.printer @@ show_program pp_meta semant_ast;
      let con = Llvm.global_context () in
      let mdl = Llvm.create_module con "llpdc" in (* TODO: name after input file name *)
      if config.gen then begin
        let post_gen = Gen.generate config.filename config.optimize config.debug_symbols mdl semant_ast
        in begin match post_gen with
          | Error err -> config.printer @@ error_string err get_lines
          | Success (_, _gen_ast) when config.optimize ->
            let passManager = Llvm.PassManager.create () in
            (* SET UP OPTIMIZATION PASSES *)
            (*  allocas to registers *)
            Llvm_scalar_opts.add_memory_to_register_promotion passManager;
            (*  simplify adjacent instructions *)
            Llvm_scalar_opts.add_instruction_combination passManager;
            (*  reassociate for better constant propagation *)
            Llvm_scalar_opts.add_reassociation passManager;
            (*  global value numbering / common subex elimination *)
            Llvm_scalar_opts.add_gvn passManager;
            (*  simplification of the cfg. DCE + block merging *)
            Llvm_scalar_opts.add_cfg_simplification passManager;
            (* COMPLETED OPTIMIZATION PASS SETUP *)
            ignore @@ Llvm.PassManager.run_module mdl passManager;
            Llvm.PassManager.dispose passManager
          | _ -> ()
        end
      end;
      if config.dump_ir then Llvm.dump_module mdl;
      if config.dispose_mdl then (
        Llvm.dispose_module mdl;
        Llvm.dispose_context con;
        None
      ) else
        Some mdl
  end

let compile config input =
  try
    let (buf, get_lines) = Setup.make_buf input in
    try generate config (Parser.program Lexer.token buf) get_lines with
    | Lexer.SyntaxError msg ->
      config.printer @@ sprintf "lexer error: %s\n" msg;
      exit 1
    | Parser.Error ->
      let context = generate_context get_lines buf.lex_start_p buf.lex_curr_p in
      config.printer @@ sprintf "%s: parser error\n" context;
      exit 1;
  with
  | Sys_error msg ->
    config.printer msg;
    exit 1