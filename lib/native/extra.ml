
open Llvm

type llmetadata

type difile_args = {
  basename: string;
  directory: string
}

external difile: llmodule -> difile_args -> llmetadata = "extra_difile"

type disubprogram_args = {
  scope: llmetadata;
  name: string;
  linkage_name: string;
  file: llmetadata;
  line_no: int;
  ty: llmetadata;
  scope_line: int
}

external disubprogram: llmodule -> disubprogram_args -> llmetadata = "extra_disubprogram"

type dilexicalblock_args = {
  scope: llmetadata;
  file: llmetadata;
  line: int;
  col: int
}

external dilexicalblock: llmodule -> dilexicalblock_args -> llmetadata = "extra_dilexicalblock"

type dilocalvariable_args = {
  scope: llmetadata;
  name: string;
  file: llmetadata;
  line_no: int;
  ty: llmetadata
}

external dilocalvariable: llmodule -> dilocalvariable_args -> llmetadata = "extra_dilocalvariable"

external get_dbg_declare: llmodule -> llvalue = "extra_get_dbg_declare"

external empty_diexpression: llmodule -> llmetadata = "extra_empty_diexpression"

external unspecified_ditype: llmodule -> string -> llmetadata = "extra_unspecified_ditype"

external metadata_to_value: llcontext -> llmetadata -> llvalue = "extra_metadata_to_value"

external mdnull: unit -> llmetadata = "extra_mdnull"

external disubroutine_type: llmodule -> llmetadata array -> llmetadata = "extra_disubroutine_type"