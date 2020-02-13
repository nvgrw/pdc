open Common.AST
open Common.VisitorMonad
open Common.Meta
open Lexing
open PassContext

open Printf

let generate (p: meta program) = 
  let post_semant = Semant.check p
  in match post_semant with
  | Success (_, final_ast) -> print_endline @@ show_program pp_meta final_ast
  | Error err -> print_endline (match err with
      | TypeError (IncompatibleBinOp (expr, lt, op, rt)) -> 
        sprintf "%s incompatible with types %s and %s." (show_binop pp_meta op) (show_typ pp_meta lt) (show_typ pp_meta rt)
      | TypeError (IncompatibleUnOp (expr, op, t)) -> 
        sprintf "%s incompatible with type %s." (show_unop pp_meta op) (show_typ pp_meta t)
      | TypeError (IncompatibleAssignment (stmt, ltyp, typ)) ->
        sprintf "incompatible assignment between types %s and %s." (show_typ pp_meta ltyp) (show_typ pp_meta typ)
      | TypeError (IfRequiresBoolean stmt)->
        "if statement requires condition to evaluate to boolean expression."
      | TypeError (WhileRequiresBoolean stmt) ->
        "while statement requires condition to evaluate to boolean expression."
      | TypeError (DoRequiresBoolean stmt) ->
        "do statement requires condition to evaluate to a boolean expression."
      | TypeError (UntypedSubExpressions expr) ->
        sprintf "untyped subexpressions in expression:\n%s" (show_expr pp_meta expr)
      | TypeError (UntypedSubLocations loc) ->
        sprintf "untyped sublocations in location:\n%s." (show_loc pp_meta loc)
      | TypeError (UntypedStatementFragment stmt) ->
        sprintf "untyped statement fragment %s." (show_stmt pp_meta stmt)
      | StructuralError (BadIdentifier ident) -> 
        sprintf "identifier `%s' not declared in scope." ident
      | StructuralError (DuplicateIdentifier ident) -> 
        sprintf "identifier `%s' already declared in scope." ident
      | Message m -> m
    ) 

let get_pos (buf: Lexing.lexbuf) = 
  let pos = buf.lex_curr_p in
  (pos.pos_fname, pos.pos_lnum, (pos.pos_cnum - pos.pos_bol + 1))

let print_pos out_channel p = 
  match p with (file, line, col) -> 
    let file = match file with | "" -> "<no file>" | _ -> file in
    fprintf out_channel "%s:%d:%d" file line col

let get_context buf get_line = 
  let pos = buf.lex_curr_p in
  let lnum = pos.pos_lnum in
  let line_number_str = sprintf "%d |" lnum in
  let pointer_pos = pos.pos_cnum - pos.pos_bol - 1 + String.length line_number_str in
  let pointer = String.make pointer_pos  ' ' ^ "^" in
  sprintf "%s%s\n%s" line_number_str (get_line (lnum - 1)) pointer

let compile buf get_line = 
  try generate (Parser.program Lexer.token buf) with
  | Lexer.SyntaxError msg -> print_endline msg
  | Parser.Error -> 
    print_endline @@ get_context buf get_line;
    printf "%a: parser error\n" print_pos (get_pos buf)