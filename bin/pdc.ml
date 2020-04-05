open Arg
open Setup

module Option = Base.Option

(* Main *)
let () =
  (* State *)
  let in_file = ref None in
  let out_file = ref "a.out" in
  let optimize = ref default_conf.optimize in
  let gen = ref default_conf.gen in
  let dump_lex_ast = ref default_conf.dump_lex_ast in
  let dump_semant_ast = ref default_conf.dump_semant_ast in
  let dump_ir = ref default_conf.dump_ir in

  (* Argument Parsing *)
  let options: (doc * spec * doc) list = [
    ("-o", Set_string out_file, "output file name");
    ("--no-opt", Clear optimize, "disable optimizations");
    ("--no-gen", Clear gen, "disable code generation and output");
    ("--dump-lex-ast", Set dump_lex_ast, "dump the lex ast to standard output");
    ("--dump-semant-ast", Set dump_semant_ast, "dump the semant ast to standard output");
    ("--dump-ir", Set dump_ir, "dump the ir to standard output");
  ] in
  let anon_handle x = in_file := Some x in
  let exec_name = Sys.argv.(0) in
  let () = parse options anon_handle (Printf.sprintf "%s [-o output_file] [--no-opt] [--no-gen] [--dump-lex-ast] [--dump-semant-ast] [--dump-ir] <file.pd>\n" exec_name) in
  let in_file = match !in_file with
    | None ->
      prerr_endline "no input file provided! aborting...";
      exit 1
    | Some f -> f
  in

  (* Setup *)
  let config = { optimize = !optimize; dump_ir = !dump_ir; dump_semant_ast = !dump_semant_ast; dump_lex_ast = !dump_lex_ast; gen = !gen } in
  let input = InFile in_file  in
  let output = OutFile !out_file in
  Compile.compile config input output

(* x = true; *)