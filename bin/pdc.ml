open Arg
open Setup

module Option = Base.Option

(* Main *)
let () =
  let () = prerr_endline "  *** PDC ***" in

  (* State *)
  let in_file = ref None in
  let out_file = ref "a.out" in
  let optimize = ref default_conf.optimize in
  let dump_ir = ref default_conf.dump_ir in

  (* Argument Parsing *)
  let options: (doc * spec * doc) list = [
    ("-o", Set_string out_file, "output file name");
    ("--dump-ir", Set dump_ir, "dump the ir to standard output");
    ("--no-opt", Clear optimize, "disable optimizations")
  ] in
  let anon_handle x = in_file := Some x in
  let exec_name = Sys.argv.(0) in
  let () = parse options anon_handle (Printf.sprintf "%s [-o output_file] [--dump-ir] <file.pd>\n" exec_name) in
  let in_file = match !in_file with
    | None ->
      prerr_endline "no input file provided! aborting...";
      exit 1
    | Some f -> f
  in

  (* Setup *)
  let config = { optimize = !optimize; dump_ir = !dump_ir } in
  let input = InFile in_file  in
  let output = OutFile !out_file in
  Compile.compile config input output

(* x = true; *)