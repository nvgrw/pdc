open Program
open Lexing

open Printf

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

let get_line ?(surround = 5) buf = 
  let pos = buf.lex_curr_p in 
  let start_pos = max pos.pos_bol (pos.pos_cnum - surround) in
  let length = pos.pos_cnum - pos.pos_bol + surround in
  let substr = Lexing.sub_lexeme buf start_pos length in
  let pointer = (String.make (pos.pos_cnum - start_pos) ' ') ^ "^" in
  sprintf "... %s ...\n    %s" substr pointer

let compile buf = 
  try generate (Parser.program Lexer.token buf) with
  | Lexer.SyntaxError msg -> print_endline msg
  | Parser.Error -> 
    print_endline @@ get_line buf;
    printf "%a: parser error\n" print_pos (get_pos buf)