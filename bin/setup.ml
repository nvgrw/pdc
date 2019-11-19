type compile_setup =
  | File of string
  (* Maybe here we can have different types of configuration, options, etc *)

type compile_conf = 
  { 
    parseOnly: bool 
  }