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
    | Error of string

  (* Lexer *)
  module Make (M : sig 
                val return: token -> lex_result
                val bind: lex_result -> (unit -> lex_result) -> lex_result

                (* Effects *)
                val fail: string -> lex_result
                val on_refill: Lexing.lexbuf -> lex_result
              end) = struct
    
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
  | white | newline    { Success [] }
  | "{"     { M.return SCOPE_OPEN } 
  | "}"     { M.return SCOPE_CLSE }
  | "["     { M.return DEREF_OPEN }
  | "]"     { M.return DEREF_CLSE }
  | "("     { M.return GROUP_OPEN }
  | ")"     { M.return GROUP_CLOSE }
  | ";"     { M.return STAT_SEPA }
  | "="     { M.return ASSIGN }
  | "if"    { M.return IF }
  | "else"  { M.return ELSE }
  | "while" { M.return WHILE }
  | "do"    { M.return DO }
  | "break" { M.return BREAK }
  | "||"    { M.return OR }
  | "&&"    { M.return AND }
  | "=="    { M.return EQ }
  | "!="    { M.return NEQ }
  | "<"     { M.return LT }
  | "<="    { M.return LEQ }
  | ">="    { M.return GEQ }
  | ">"     { M.return GT }
  | "+"     { M.return PLUS }
  | "-"     { M.return MINUS }
  | "*"     { M.return MULTIPLY }
  | "/"     { M.return DIVIDE }
  | "!"     { M.return NOT }
  | num as n    { M.return (NUM (int_of_string n)) }
  | real as r    { M.return (REAL (float_of_string r)) }
  | "true"  { M.return TRUE }
  | "false" { M.return FALSE }
  | ident as id { M.return (IDENT id) }
  | _
    { M.fail ("bad token " ^ (Lexing.lexeme lexbuf)) }

{
  let rec lex buf k = let tok = token buf in (match tok with Success toks -> let () = k toks in M.bind (token buf) (fun () -> lex buf k))

  end
}