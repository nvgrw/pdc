type compile_setup =
  | File of string
  | String of string
  (* Maybe here we can have different types of configuration, options, etc *)

type compile_conf = 
  { 
    parseOnly: bool 
  }

let default_conf = { parseOnly = false }

let make_buf = function
  | File path -> (
      Lexing.from_string path, 
      fun _ _ -> ["todo -- implement"]
    )
  | String code -> (
      Lexing.from_string code, 
      fun lstart lend ->
        Core.List.slice (Core.String.split_lines code) lstart (lend + 1)
    )