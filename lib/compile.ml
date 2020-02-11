open Common.AST
open Common.VisitorMonad
open Lexing

open Printf

let generate (p: program) = 
  let post_semant = Semant.check p
  in match post_semant with
  | Success (_, final_ast) -> print_endline @@ show_program final_ast
  | Error err -> print_endline (match err with
      | TypeError (IncompatibleBinOp (lt, op, rt)) -> 
        Printf.sprintf "%s incompatible with types %s and %s." (show_binop op) (show_typ lt) (show_typ rt)
      | TypeError (IncompatibleUnOp (op, t)) -> 
        Printf.sprintf "%s incompatible with type %s." (show_unop op) (show_typ t)
      | TypeError (IncompatibleAssignment (ltyp, typ)) ->
        Printf.sprintf "incompatible assignment between types %s and %s." (show_typ ltyp) (show_typ typ)
      | TypeError IfRequiresBoolean ->
        "if statement requires condition to evaluate to boolean expression."
      | TypeError WhileRequiresBoolean ->
        "while statement requires condition to evaluate to boolean expression."
      | TypeError DoRequiresBoolean ->
        "do statement requires condition to evaluate to a boolean expression."
      | TypeError (UntypedSubExpressions expr) ->
        Printf.sprintf "untyped subexpressions in expression:\n%s" (show_expr expr)
      | TypeError (UntypedSubLocations loc) ->
        Printf.sprintf "untyped sublocations in location:\n%s." (show_loc loc)
      | TypeError (UntypedStatementFragment stmt) ->
        Printf.sprintf "untyped statement fragment %s." (show_stmt stmt)
      | StructuralError (BadIdentifier ident) -> 
        Printf.sprintf "identifier `%s' not declared in scope." ident
      | StructuralError (DuplicateIdentifier ident) -> 
        Printf.sprintf "identifier `%s' already declared in scope." ident
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