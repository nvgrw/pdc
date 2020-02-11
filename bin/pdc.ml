open Arg
open Setup

(* Main *)
let () = 
  let () = print_endline "This is PDC, your friendly neighborhood Probabilistic Dragon Compiler!" in
  let options: (doc * spec * doc) list = [
    ("--parse", String print_endline, "this is the help text i think") 
  ] in
  let anon_handle x = print_endline @@ Printf.sprintf "Anonymous argument %s" x in
  let exec_name = Sys.argv.(0) in
  let () = parse options anon_handle (Printf.sprintf "%s [--parse]\n" exec_name) in
  let input_string = String {test|{
  int i; int j; float v; float x; float[100] a; float[100][50] b;
  while( true ) {
      do i = i + 1; while( a[i] < v);
      do j = j - 1; while( a[j] > v);
      if( i >= j ) break;
      x = a[i]; a[i] = a[j] * b[i][j]; a[j] = x;
      do { j = j + 1; } while (true);
  }
}|test} in
  match make_buf input_string with
    (buf, get_line) -> Compile.compile buf get_line 

(* x = true; *)