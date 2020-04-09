open Arg
open Setup

module Option = Base.Option

(* Main *)
let () =
  (* State *)
  let in_file = ref None in
  let out_file = ref "a.out" in
  let config = default_conf in

  (* Argument Parsing *)
  let options: (doc * spec * doc) list = [
    ("-o", Set_string out_file, "output file name");

    ("--no-opt", Unit (fun () -> config.optimize <- false), "disable optimizations");
    ("--no-gen", Unit (fun () -> config.gen <- false), "disable code generation and output");


    ("--dump-lex-ast", Unit (fun () -> config.dump_lex_ast <- true), "dump the lex ast to standard output");
    ("--dump-semant-ast", Unit (fun () -> config.dump_semant_ast <- true), "dump the semant ast to standard output");
    ("--dump-ir", Unit (fun () -> config.dump_ir <- true), "dump the ir to standard output");
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
  let input = InFile in_file  in
  let output = OutFile !out_file in
  ignore @@ Compile.compile config input output