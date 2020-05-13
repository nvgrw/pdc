
open Llvm

type llmetadata

module DIFile = struct
  type args = {
    basename: string;
    directory: string
  }
end

external difile: llmodule -> DIFile.args -> llmetadata = "extra_difile"

module DISubprogram = struct
  type args = {
    scope: llmetadata;
    name: string;
    linkage_name: string;
    file: llmetadata;
    line_no: int;
    ty: llmetadata;
    scope_line: int;
    unit: llmetadata;
  }
end

external disubprogram: llmodule -> DISubprogram.args -> llmetadata = "extra_disubprogram"

module DILexicalBlock = struct
  type args = {
    scope: llmetadata;
    file: llmetadata;
    line: int;
    col: int
  }
end

external dilexicalblock: llmodule -> DILexicalBlock.args -> llmetadata = "extra_dilexicalblock"

module DILocalVariable = struct
  type args = {
    scope: llmetadata;
    name: string;
    file: llmetadata;
    line_no: int;
    ty: llmetadata
  }
end

external dilocalvariable: llmodule -> DILocalVariable.args -> llmetadata = "extra_dilocalvariable"

module DICompileUnit = struct
  type args = {
    file: llmetadata;
    producer: string;
    isOptimized: bool;
  }
end

external dicompileunit: llmodule -> DICompileUnit.args -> llmetadata = "extra_dicompileunit"

external get_dbg_declare: llmodule -> llvalue = "extra_get_dbg_declare"

external get_dbg_value: llmodule -> llvalue = "extra_get_dbg_value"

external empty_diexpression: llmodule -> llmetadata = "extra_empty_diexpression"

external diexpression: llmodule -> int array -> llmetadata = "extra_diexpression"

external unspecified_ditype: llmodule -> string -> llmetadata = "extra_unspecified_ditype"

external metadata_to_value: llcontext -> llmetadata -> llvalue = "extra_metadata_to_value"

external mdnull: unit -> llmetadata = "extra_mdnull"

external disubroutine_type: llmodule -> llmetadata array -> llmetadata = "extra_disubroutine_type"

external value_to_metadata: llvalue -> llmetadata = "extra_value_to_metadata"

module AttachInstLocation = struct
  type args = {
    line: int;
    col: int;
    scope: llmetadata;
  }
end

external attach_inst_location: llmodule -> llvalue -> AttachInstLocation.args -> unit = "extra_attach_inst_location"

external extra_add_int_module_flag: llmodule -> string -> int -> unit = "extra_add_int_module_flag"
