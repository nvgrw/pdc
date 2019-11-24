open Program
open Lexing

open Printf

type printable_token = 
  | WHILE
  | TRUE
  | STAT_SEPA
  | SCOPE_OPEN
  | SCOPE_CLSE
  | REAL of (float)
  | PLUS
  | OR
  | NUM of (int)
  | NOT
  | NEQ
  | MULTIPLY
  | MINUS
  | LT
  | LEQ
  | INT
  | IF
  | IDENT of (string)
  | GT
  | GROUP_OPEN
  | GROUP_CLSE
  | GEQ
  | FLOAT
  | FALSE
  | EQ
  | EOF
  | ELSE
  | DO
  | DIVIDE
  | DEREF_OPEN
  | DEREF_CLSE
  | CHAR
  | BREAK
  | BOOL
  | ASSIGN
  | AND
[@@deriving show]

let show_token (tok: Parser.token): string = show_printable_token (match tok with
    | Parser.WHILE -> WHILE
    | Parser.TRUE -> TRUE
    | Parser.STAT_SEPA -> STAT_SEPA
    | Parser.SCOPE_OPEN -> SCOPE_OPEN
    | Parser.SCOPE_CLSE -> SCOPE_CLSE
    | Parser.REAL v -> REAL v
    | Parser.PLUS -> PLUS
    | Parser.OR -> OR
    | Parser.NUM v -> NUM v
    | Parser.NOT -> NOT
    | Parser.NEQ -> NEQ
    | Parser.MULTIPLY -> MULTIPLY
    | Parser.MINUS -> MINUS
    | Parser.LT -> LT
    | Parser.LEQ -> LEQ
    | Parser.INT -> INT
    | Parser.IF -> IF
    | Parser.IDENT v -> IDENT v
    | Parser.GT -> GT
    | Parser.GROUP_OPEN -> GROUP_OPEN
    | Parser.GROUP_CLSE -> GROUP_CLSE
    | Parser.GEQ -> GEQ
    | Parser.FLOAT -> FLOAT
    | Parser.FALSE -> FALSE
    | Parser.EQ -> EQ
    | Parser.EOF -> EOF
    | Parser.ELSE -> ELSE
    | Parser.DO -> DO
    | Parser.DIVIDE -> DIVIDE
    | Parser.DEREF_OPEN -> DEREF_OPEN
    | Parser.DEREF_CLSE -> DEREF_CLSE
    | Parser.CHAR -> CHAR
    | Parser.BREAK -> BREAK
    | Parser.BOOL -> BOOL
    | Parser.ASSIGN -> ASSIGN
    | Parser.AND -> AND
  )

type compile_result = (program, string) result
type position = string * int * int

let generate (p: program) = print_endline @@ show_program p

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

let rec tokenize buf = 
  let next = Lexer.token buf in match next with
  | EOF -> []
  | _ as tok -> tok :: tokenize buf

let parse buf = 
  try generate (Parser.program Lexer.token buf) with
  | Lexer.SyntaxError msg -> print_endline msg
  | Parser.Error -> 
    print_endline @@ get_line buf;
    printf "%a: parser error\n" print_pos (get_pos buf)

let compile buf tokensOnly = 
  if tokensOnly then 
    print_endline @@ String.concat ", " @@ List.map show_token @@ tokenize buf
  else 
    parse buf