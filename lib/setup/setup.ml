type compile_in =
  | InFile of string
  | InChannel of in_channel
  | InString of string

type compile_out =
  | OutFile of string
  | OutChannel of out_channel

(* Maybe here we can have different types of configuration, options, etc *)
type compile_conf =
  {
    optimize: bool;
    dump_ir: bool;
    dump_semant_ast: bool;
    dump_lex_ast: bool;
    gen: bool
  }

let default_conf = { optimize = true; dump_ir = false; dump_semant_ast = false; dump_lex_ast = false; gen = true }

let make_buf = function
  | InFile path ->
    (
      begin
        let file = open_in path in
        try
          let buf = Lexing.from_channel file in
          buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = path }; buf
        with e ->
          close_in file;
          raise e
      end,
      fun lstart lend ->
        Core.List.slice (Core.In_channel.read_lines path) lstart (lend + 1)
    )
  | InString code -> (
      begin
        let buf = Lexing.from_string code in
        buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = "<no file>" }; buf
      end,
      fun lstart lend ->
        Core.List.slice (Core.String.split_lines code) lstart (lend + 1)
    )
  | _ -> assert false