
open Llvm

external extra_difile : llcontext -> string -> string -> llvalue = "extra_difile"

external extra_dilocalvariable: llmodule -> string -> llvalue -> int -> llvalue = "extra_dilocalvariable"

external extra_build_declare: llvalue -> llvalue -> llbuilder -> llmodule -> llvalue = "extra_build_declare"