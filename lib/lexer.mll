{
  type token = 
    (* Structural *)
    | SCOPE_OPEN
    | SCOPE_CLSE
    | DEREF_OPEN
    | DEREF_CLSE
    | GROUP_OPEN
    | GROUP_CLOSE
    | STAT_SEPA
    (* Misc *)
    | ASSIGN
    | IDENT of string
    (* Control Flow *)
    | IF
    | ELSE
    | WHILE
    | DO
    | BREAK
    (* Relational *)
    | OR | AND
    | EQ | NEQ
    | LT | LEQ | GEQ | GT
    (* Arithmetic *)
    | PLUS | MINUS | MULTIPLY | DIVIDE
    (* Unary *)
    | NOT (* | NEGATE -- negate is ambiguous, will handle this in parsing *)
    | NUM of int
    | REAL of float
    | TRUE
    | FALSE
    (* Types *)
    | INT | FLOAT | CHAR | BOOL
    [@@deriving show]

  type lex_result =
    | Success of token list
    | Incomplete of token list
    | Error of string

  (* Lexer *)
  module Make (M : sig 
                val return: token list -> lex_result
                val bind: lex_result -> (token list -> lex_result) -> lex_result

                (* Effects *)
                val fail: string -> lex_result

                val complete: lex_result
                (* val on_refill: Lexing.lexbuf -> lex_result *)
              end) = struct
    
    let append t = M.return [t]
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
  | white | newline { M.return [] }
  | "{"     { append SCOPE_OPEN } 
  | "}"     { append SCOPE_CLSE }
  | "["     { append DEREF_OPEN }
  | "]"     { append DEREF_CLSE }
  | "("     { append GROUP_OPEN }
  | ")"     { append GROUP_CLOSE }
  | ";"     { append STAT_SEPA }
  | "="     { append ASSIGN }
  | "if"    { append IF }
  | "else"  { append ELSE }
  | "while" { append WHILE }
  | "do"    { append DO }
  | "break" { append BREAK }
  | "||"    { append OR }
  | "&&"    { append AND }
  | "=="    { append EQ }
  | "!="    { append NEQ }
  | "<"     { append LT }
  | "<="    { append LEQ }
  | ">="    { append GEQ }
  | ">"     { append GT }
  | "+"     { append PLUS }
  | "-"     { append MINUS }
  | "*"     { append MULTIPLY }
  | "/"     { append DIVIDE }
  | "!"     { append NOT }
  | num as n { append (NUM (int_of_string n)) }
  | real as r { append (REAL (float_of_string r)) }
  | "true"  { append TRUE }
  | "false" { append FALSE }
  | ident as id { append (IDENT id) }
  | eof { M.complete }
  | _
    { M.fail ("bad token " ^ (Lexing.lexeme lexbuf)) }

{
  let rec lex buf = M.bind (token buf) (fun _ -> lex buf)

  end
}