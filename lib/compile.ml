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
      | TypeError (UntypedSubExpressions expr) ->
        Printf.sprintf "untyped subexpressions in expression:\n%s" (show_expr expr)
      | TypeError (UntypedSubLocations loc) ->
        Printf.sprintf "untyped sublocations in location:\n%s" (show_loc loc)
      | StructuralError (BadIdentifier ident) -> 
        Printf.sprintf "identifier `%s' not declared in scope." ident
      | Message m -> m
    ) 

let get_pos (buf: Lexing.lexbuf) = 
  let pos = buf.lex_curr_p in
  (pos.pos_fname, pos.pos_lnum, (pos.pos_cnum - pos.pos_bol + 1))

let print_pos out_channel p = 
  match p with (file, line, col) -> 
    let file = match file with | "" -> "<no file>" | _ -> file in
    fprintf out_channel "%s:%d:%d" file line col

(* TODO: seek to beginning of token *)
let get_line ?(surround = 5) buf = 
  let pos = buf.lex_curr_p in 
  let start_pos = max pos.pos_bol (pos.pos_cnum - surround) in
  let length = pos.pos_cnum - pos.pos_bol + surround in
  let substr = Lexing.sub_lexeme buf start_pos length in
  let pointer = (String.make (pos.pos_cnum - start_pos - 1) ' ') ^ "^" in
  sprintf "%s\n%s" substr pointer

(* let rec tokenize buf = 
   let next = Lexer.token buf in match next with
   | EOF -> []
   | _ as tok -> tok :: tokenize buf *)

let compile buf = 
  try generate (Parser.program Lexer.token buf) with
  | Lexer.SyntaxError msg -> print_endline msg
  | Parser.Error -> 
    print_endline @@ get_line buf;
    printf "%a: parser error\n" print_pos (get_pos buf)