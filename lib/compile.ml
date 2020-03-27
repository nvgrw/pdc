open Common.AST
open Common.VisitorMonad
open Common.Meta

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

let generate (p: meta program) (get_lines: int -> int -> string list) = 
  let post_semant = Semant.check p
  in match post_semant with
  | Error err -> print_endline @@ begin
      match err with
      | TypeError (IncompatibleBinOp (expr, lt, op, rt)) -> 
        let context = (match get_meta_expr expr with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
        sprintf "%s: %s incompatible with types %s and %s." context (show_binop pp_meta op) (show_typ pp_meta lt) (show_typ pp_meta rt) 
      | TypeError (IncompatibleUnOp (expr, op, t)) -> 
        let context = (match get_meta_expr expr with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
        sprintf "%s: %s incompatible with type %s." context (show_unop pp_meta op) (show_typ pp_meta t)
      | TypeError (IncompatibleAssignment (stmt, ltyp, typ)) ->
        let context = (match get_meta_stmt stmt with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
        sprintf "%s: incompatible assignment between types %s and %s." context (show_typ pp_meta ltyp) (show_typ pp_meta typ)
      | TypeError (IfRequiresBoolean stmt)->
        let context = (match get_meta_stmt stmt with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
        sprintf "%s: if statement requires condition to evaluate to boolean expression." context
      | TypeError (WhileRequiresBoolean stmt) ->
        let context = (match get_meta_stmt stmt with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
        sprintf "%s: while statement requires condition to evaluate to boolean expression." context
      | TypeError (DoRequiresBoolean stmt) ->
        let context = (match get_meta_stmt stmt with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
        sprintf "%s: do statement requires condition to evaluate to a boolean expression." context
      | TypeError (UntypedSubExpressions expr) ->
        let context = (match get_meta_expr expr with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
        sprintf "%s: untyped subexpressions in expression %s." context (show_expr pp_meta expr)
      | TypeError (UntypedSubLocations loc) ->
        let context = (match get_meta_loc loc with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
        sprintf "%s: untyped sublocations in location %s." context (show_loc pp_meta loc)
      | TypeError (UntypedStatementFragment stmt) ->
        let context = (match get_meta_stmt stmt with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
        sprintf "%s: untyped statement fragment %s." context (show_stmt pp_meta stmt)
      | StructuralError (BadIdentifier (m, ident)) -> 
        let context = (match m with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
        sprintf "%s: identifier `%s' not declared in scope." context ident
      | StructuralError (DuplicateIdentifier (decl, ident)) -> 
        let context = (match get_meta_decl decl with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
        sprintf "%s: identifier `%s' already declared in scope." context ident
      | CodegenError (CannotGenerateExpression expr) ->
        let context = (match get_meta_expr expr with Position (s_pos, e_pos) -> generate_context get_lines s_pos e_pos) in
        sprintf "%s: cannot generate expression %s." context (show_expr pp_meta expr)
      | Message m -> m
    end
  | Success (_, semant_ast) -> 
    let post_gen = Gen.generate semant_ast
    in begin 
      match post_gen with
      | Error err -> print_endline @@ begin 
          match err with
          | Message m -> m
          | _ -> "no message"
        end
      | Success (_, gen_ast) ->
        print_endline @@ show_program pp_meta gen_ast 
    end

let compile buf get_lines = 
  try generate (Parser.program Lexer.token buf) get_lines with
  | Lexer.SyntaxError msg -> print_endline msg
  | Parser.Error -> 
    let context = generate_context get_lines buf.lex_start_p buf.lex_curr_p in
    printf "%s: parser error\n" context