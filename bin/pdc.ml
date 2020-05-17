open Arg
open Setup

module Option = Base.Option

type output_type =
  | Executable
  | Object
  | Bitcode

let prefix = [%blob "../.prefix"]
let clang_stdlib_args = Printf.sprintf "-L%s/lib -lpdstd -Xclang -load -Xclang %s/lib/libA4RRP.so" prefix prefix

(* Main *)
let () =
  (* State *)
  let in_file = ref None in
  let out_file = ref "a.out" in
  let out_type = ref Executable in
  let config = default_conf in

  (* Argument Parsing *)
  let options: (doc * spec * doc) list = [
    ("-c", Unit (fun () -> out_type := Object), "compile to target object file");
    ("-o", Set_string out_file, "output file name");

    ("-O0", Unit (fun () -> config.optimize <- false), "disable optimizations");
    ("-g", Unit (fun () -> config.debug_symbols <- true), "enable debug symbols");
    ("--no-gen", Unit (fun () -> config.gen <- false), "disable code generation and output");


    ("--dump-lex-ast", Unit (fun () -> config.dump_lex_ast <- true), "dump the lex ast to standard output");
    ("--dump-semant-ast", Unit (fun () -> config.dump_semant_ast <- true), "dump the semant ast to standard output");
    ("--dump-ir", Unit (fun () -> config.dump_ir <- true), "dump the ir to standard output");
  ] in
  let anon_handle x = in_file := Some x in
  let exec_name = Sys.argv.(0) in
  let () = parse options anon_handle (Printf.sprintf "%s [-c] [-o output_file] [--no-opt] [-O0] [-g] [--dump-lex-ast] [--dump-semant-ast] [--dump-ir] <file.pd>\n" exec_name) in
  let in_file = match !in_file with
    | None ->
      prerr_endline "no input file provided! aborting...";
      exit 1
    | Some f -> f
  in
  if (!out_type == Executable &&
      Core.String.is_suffix !out_file ~suffix:".bc") then
    out_type := Bitcode;

  (* Setup *)
  let input = InFile in_file in
  config.dispose_mdl <- false;
  config.filename <- Some in_file;
  match Compile.compile config input with
  | None -> ()
  | Some mdl ->
    if not @@ begin match !out_type with
        | Bitcode -> Llvm_bitwriter.write_bitcode_file mdl !out_file
        | Executable ->
          let clang = Unix.open_process_out @@ Printf.sprintf "clang -x ir - %s -o \"%s\"" clang_stdlib_args !out_file in
          let status = Llvm_bitwriter.output_bitcode ~unbuffered:false clang mdl in
          (match Unix.close_process_out clang with
           | WEXITED clang_exit_code -> clang_exit_code == 0 && status
           | _ -> false)
        | Object ->
          let clang = Unix.open_process_out @@ Printf.sprintf "clang -c -x ir - -o \"%s.o\"" !out_file in
          let status = Llvm_bitwriter.output_bitcode ~unbuffered:false clang mdl in
          match Unix.close_process_out clang with
          | WEXITED clang_exit_code -> clang_exit_code == 0 && status
          | _ -> false
      end then
      prerr_endline "[front end] export failed";
    Llvm.dispose_module mdl