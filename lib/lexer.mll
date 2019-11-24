{
  open Lexing
  open Parser

  exception SyntaxError of string

  let next_line buf =
    let curr_pos = buf.lex_curr_p in
    buf.lex_curr_p <- { 
      curr_pos with pos_bol = buf.lex_curr_pos; 
                    pos_lnum = curr_pos.pos_lnum + 1 
    }
}

let white = [' ' '\t']+
(* to allow us to track the line number later *)
let newline = '\n' | '\r' | "\r\n"
let digit = ['0'-'9']
let ident = ['a'-'z' 'A'-'Z'] (['a'-'z' 'A'-'Z']|digit)*

let num = digit+

let frac = '.' digit+
let real = digit+ | (digit* frac)

rule token = parse
  | white       { token lexbuf }
  | newline     { next_line lexbuf; token lexbuf }
  | "{"         { SCOPE_OPEN } 
  | "}"         { SCOPE_CLSE }
  | "["         { DEREF_OPEN }
  | "]"         { DEREF_CLSE }
  | "("         { GROUP_OPEN }
  | ")"         { GROUP_CLSE }
  | ";"         { STAT_SEPA }
  | "="         { ASSIGN }
  | "if"        { IF }
  | "else"      { ELSE }
  | "while"     { WHILE }
  | "do"        { DO }
  | "break"     { BREAK }
  | "||"        { OR }
  | "&&"        { AND }
  | "=="        { EQ }
  | "!="        { NEQ }
  | "<"         { LT }
  | "<="        { LEQ }
  | ">="        { GEQ }
  | ">"         { GT }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "*"         { MULTIPLY }
  | "/"         { DIVIDE }
  | "!"         { NOT }
  | num as n    { (NUM (int_of_string n)) }
  | real as r   { (REAL (float_of_string r)) }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | ident as id { IDENT id }
  | eof         { EOF }
  | _
    { raise (SyntaxError (Printf.sprintf "Unexpected character `%s'" @@ lexeme lexbuf)) }