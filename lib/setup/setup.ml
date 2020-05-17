type compile_in =
  | InFile of string
  | InChannel of in_channel
  | InString of string

type compile_out = Llvm.llmodule option

(* Maybe here we can have different types of configuration, options, etc *)
type compile_conf =
  {
    mutable optimize: bool;
    mutable debug_symbols: bool;
    mutable dump_ir: bool;
    mutable dump_semant_ast: bool;
    mutable dump_lex_ast: bool;
    mutable gen: bool;
    mutable dispose_mdl: bool;
    mutable printer: string -> unit;
    mutable filename: string option
  }

let default_conf = {
  optimize = true;
  debug_symbols = false;
  gen = true;
  dump_lex_ast = false;
  dump_semant_ast = false;
  dump_ir = false;
  dispose_mdl = true;
  printer = prerr_endline;
  filename = None
}

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