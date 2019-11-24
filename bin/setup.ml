type compile_setup =
  | File of string
  | String of string
  (* Maybe here we can have different types of configuration, options, etc *)

type compile_conf = 
  { 
    tokensOnly: bool;
    parseOnly: bool 
  }

let default_conf = { tokensOnly = false; parseOnly = false }

let make_buf = function
  | File path -> Lexing.from_string path
  | String code -> Lexing.from_string code