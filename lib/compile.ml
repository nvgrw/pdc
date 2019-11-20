open Lexer
open List

type lexer_result =
  | Success of token list
  | Error of string

module LexerMake_list = Make(struct
    type 'a t = 'a list

    let return (x: 'a): 'a list = [x]

    let bind (a: 'a list) (f: 'a -> 'b list): 'b list = List.flatten (List.map f a)

    let fail (message: string): 'a t = print_endline message; []

    let on_refill (_: Lexing.lexbuf) : unit t = []
  end)

let compile buf: unit = 
  let tokenize = LexerMake_list.token in 
  let tokens: token list = tokenize buf in 
  print_endline @@ String.concat ", " @@ map show_token tokens