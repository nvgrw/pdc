open Arg
(* open Setup *)

(* Main *)
let () = 
  print_endline "This is PDC, your friendly neighborhood Probabilistic Dragon Compiler!";

  let options: (doc * spec * doc) list = [
    ("--parse", String print_endline, "this is the help text i think") 
  ] in
  let anon_handle x = print_endline @@ Printf.sprintf "Anonymous argument %s" x in
  let exec_name = Sys.argv.(0) in
  parse options anon_handle (Printf.sprintf "%s [--parse]\n" exec_name)