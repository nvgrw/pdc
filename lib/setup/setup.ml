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
    dump_ir: bool
  }

let default_conf = { optimize = true; dump_ir = false }

let make_buf = function
  | InFile path ->
    (
      begin
        let file = open_in path in
        try
          Lexing.from_channel file
        with e ->
          close_in file;
          raise e
      end,
      fun lstart lend ->
        Core.List.slice (Core.In_channel.read_lines path) lstart (lend + 1)
    )
  | InString code -> (
      Lexing.from_string code,
      fun lstart lend ->
        Core.List.slice (Core.String.split_lines code) lstart (lend + 1)
    )
  | _ -> assert false