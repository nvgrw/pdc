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
    parseOnly: bool
  }

let default_conf = { parseOnly = false }

let make_buf = function
  | InFile path -> (
      Lexing.from_string path,
      fun _ _ -> ["todo -- implement"]
    )
  | InString code -> (
      Lexing.from_string code,
      fun lstart lend ->
        Core.List.slice (Core.String.split_lines code) lstart (lend + 1)
    )
  | _ -> assert false