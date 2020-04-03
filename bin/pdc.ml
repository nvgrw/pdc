open Arg
open Setup

(* Main *)
let () =
  let () = prerr_endline "  *** PDC ***" in

  (* State *)
  let out_file = ref "a.out" in
  let optimize = ref default_conf.optimize in
  let dump_ir = ref default_conf.dump_ir in

  (* Argument Parsing *)
  let options: (doc * spec * doc) list = [
    ("-o", Set_string out_file, "output file name");
    ("--dump-ir", Set dump_ir, "dump the ir to standard output");
    ("--no-opt", Clear optimize, "disable optimizations")
  ] in
  let anon_handle x = print_endline @@ Printf.sprintf "Anonymous argument %s" x in
  let exec_name = Sys.argv.(0) in
  let () = parse options anon_handle (Printf.sprintf "%s [-o output_file] [--dump-ir]\n" exec_name) in

  (* Setup *)
  let config = { optimize = !optimize; dump_ir = !dump_ir } in
  let input = InString {test|{
  int i; int j; float v; float x; float[100] a; float[100][50] b;
  while( true ) {
    do i = i + 1; while( a[i] < v);
    do j = j - 1; while( a[j] > v);
    if( i >= j ) break;
    x = a[i]; a[i] = a[j] * b[i][j]; a[j] = x;
    do { j = j + 1; } while (true);
  }
}|test} in
  let output = OutFile !out_file in
  Compile.compile config input output

(* x = true; *)