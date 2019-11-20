type compile_setup =
  | File of string
  | String of string
  (* Maybe here we can have different types of configuration, options, etc *)

type compile_conf = 
  { 
    parseOnly: bool 
  }

let make_buf = function
  | File path -> Lexing.from_string path
  | String code -> Lexing.from_string code 