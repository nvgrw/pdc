open Lexer
open List

module LexerMake = Make(struct
    let return (v: 'a list): ('a list, 'b) result = Ok v

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
  let result = tokenize buf in 
  match result with
  | Ok tokens -> print_endline @@ String.concat ", " @@ map show_token tokens
  | Error message -> print_endline @@ "Got error " ^ message