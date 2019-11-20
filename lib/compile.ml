open Lexer
open List

module LexerMake = Make(struct
    type ('a, 'b) t = ('a, 'b) result

    let return (v: 'a list): (token list, string) result = Ok v

    let bind m f = match m with
      | Error _ -> m
      | Ok tokens -> (let update = f tokens in
                      match update with 
                      | Ok new_tokens -> return @@ tokens @ new_tokens
                      | Error _ -> update)

    let fail message = Error message

    let on_refill lexbuf = return []
  end)

let compile buf: unit = 
  let tokenize = LexerMake.token in 
  let tokens: token list = tokenize buf in 
  print_endline @@ String.concat ", " @@ map show_token tokens