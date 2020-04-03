open Arg
open Setup

(* Main *)
let () =
  let () = prerr_endline "  *** PDC ***" in
  let out_file = ref "a.out" in
  let options: (doc * spec * doc) list = [
    ("--dump-ir", Unit (fun () -> ()), "dump the ir to standard output"); (* todo: implement *)
    ("-o", String ((:=) out_file), "output file name")
  ] in
  let anon_handle x = print_endline @@ Printf.sprintf "Anonymous argument %s" x in
  let exec_name = Sys.argv.(0) in
  let () = parse options anon_handle (Printf.sprintf "%s [--dump-ir] [-o]\n" exec_name) in
  let input_string = InString {test|{
  int i; int j; float v; float x; float[100] a; float[100][50] b;
  while( true ) {
    do i = i + 1; while( a[i] < v);
    do j = j - 1; while( a[j] > v);
    if( i >= j ) break;
    x = a[i]; a[i] = a[j] * b[i][j]; a[j] = x;
    do { j = j + 1; } while (true);
  }
}|test} in
  let out = OutFile !out_file in
  match make_buf input_string with
    (buf, get_lines) -> Compile.compile buf get_lines out

(* x = true; *)