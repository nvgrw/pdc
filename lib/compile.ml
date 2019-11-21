open Lexer

module LexerMake = Make(struct
    let return t = Incomplete t

    let bind m f = match m with
      | Incomplete tokens -> 
        (let update = f tokens in match update with 
          | Success new_tokens -> Success (tokens @ new_tokens)
          | Incomplete new_tokens -> return new_tokens
          | _ -> update)
      | _ -> m

    let fail message = Error message

    let complete = Success []
  end)

let compile buf: unit = 
  let result = LexerMake.lex buf in 
  match result with
  | Success tokens -> print_endline @@ String.concat ", " @@ List.map show_token tokens
  | Error message -> print_endline @@ "Got error " ^ message
  | _ -> print_endline @@ "Failed to completely process input stream"