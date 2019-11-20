open Lexer

module LexerMake = Make(struct
    let return t = Success [t]

    let bind m f = match m with
      | Error _ -> m
      | Success tokens -> 
        (let update = f () in match update with 
          | Error _ -> update
          | Success new_tokens -> Success (tokens @ new_tokens))

    let fail message = Error message

    let on_refill lexbuf = Success []
  end)

let compile buf: unit = 
  Printexc.record_backtrace true;
  let result = LexerMake.lex buf (fun tokens -> print_endline @@ String.concat ", " @@ List.map show_token tokens) in 
  match result with
  | Success tokens -> print_endline @@ String.concat ", " @@ List.map show_token tokens
  | Error message -> print_endline @@ "Got error " ^ message