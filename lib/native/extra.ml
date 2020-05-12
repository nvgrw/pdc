
open Llvm

type difile_args = {
  basename: string;
  directory: string
}

external difile: llmodule -> difile_args -> llvalue = "extra_difile"

type disubprogram_args = {
  scope: llvalue;
  name: string;
  linkage_name: string;
  file: llvalue;
  line_no: int;
  ty: llvalue;
  scope_line: int
}

external disubprogram: llmodule -> disubprogram_args -> llvalue = "extra_disubprogram"

type dilexicalblock_args = {
  scope: llvalue;
  file: llvalue;
  line: int;
  col: int
}

external dilexicalblock: llmodule -> dilexicalblock_args -> llvalue = "extra_dilexicalblock"

type dilocalvariable_args = {
  scope: llvalue;
  name: string;
  file: llvalue;
  line_no: int;
  ty: llvalue
}

external dilocalvariable: llmodule -> dilocalvariable_args -> llvalue = "extra_dilocalvariable"

external get_dbg_declare: llmodule -> llvalue = "extra_get_dbg_declare"

external empty_diexpression: llmodule -> llvalue = "extra_empty_diexpression"