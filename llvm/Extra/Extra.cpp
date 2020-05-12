
#include <vector>

//#include "caml/callback.h"
//#include "caml/custom.h"
//#include "caml/fail.h"
//#include "caml/memory.h"
#include "caml/alloc.h"
#include "caml/misc.h"

#include "llvm-c/Types.h"

#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"

//#define Builder_val(v) (*(LLVMBuilderRef *)(Data_custom_val(v)))
#define Value_val(v) (*(LLVMValueRef *)(Data_custom_val(v)))
#define VAL2META(V) (((MetadataAsValue *)V)->getMetadata())

using namespace llvm;

extern "C" {

/* llmodule -> difile_args -> llvalue */
CAMLprim LLVMValueRef extra_difile(LLVMModuleRef M, value Arguments) {
  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);

  // Extract Arguments
  const char *Basename = String_val(Field(Arguments, 0));
  const char *Directory = String_val(Field(Arguments, 1));

  return wrap(MetadataAsValue::get(Module.getContext(),
                                   DIB.createFile(Basename, Directory)));
}

/* llmodule -> disubprogram_args -> llvalue */
CAMLprim LLVMValueRef extra_disubprogram(LLVMModuleRef M, value Arguments) {
  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);

  // Extract Arguments
  auto *Scope = cast<DIScope>(VAL2META(unwrap(Value_val(Field(Arguments, 0)))));
  const char *Name = String_val(Field(Arguments, 1));
  const char *LinkageName = String_val(Field(Arguments, 2));
  auto *File = cast<DIFile>(VAL2META(unwrap(Value_val(Field(Arguments, 3)))));
  unsigned int LineNo = Unsigned_int_val(Field(Arguments, 4));
  auto *Ty =
      cast<DISubroutineType>(VAL2META(unwrap(Value_val(Field(Arguments, 5)))));
  unsigned int ScopeLine = Unsigned_int_val(Field(Arguments, 6));

  return wrap(MetadataAsValue::get(
      Module.getContext(), DIB.createFunction(Scope, Name, LinkageName, File,
                                              LineNo, Ty, ScopeLine)));
}

/* llmodule -> dilexicalblock_args -> llvalue */
CAMLprim LLVMValueRef extra_dilexicalblock(LLVMModuleRef M, value Arguments) {
  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);

  // Extract Args
  auto *Scope = cast<DIScope>(VAL2META(unwrap(Value_val(Field(Arguments, 0)))));
  auto *File = cast<DIFile>(VAL2META(unwrap(Value_val(Field(Arguments, 1)))));
  unsigned int Line = Unsigned_int_val(Field(Arguments, 2));
  unsigned int Col = Unsigned_int_val(Field(Arguments, 3));

  return wrap(MetadataAsValue::get(
      Module.getContext(), DIB.createLexicalBlock(Scope, File, Line, Col)));
}

/* llmodule -> dilocalvariable_args -> llvalue */
CAMLprim LLVMValueRef extra_dilocalvariable(LLVMModuleRef M, value Arguments) {
  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);

  // Extract Args
  auto *Scope = cast<DIScope>(VAL2META(unwrap(Value_val(Field(Arguments, 0)))));
  const char *Name = String_val(Field(Arguments, 1));
  auto *File = cast<DIFile>(VAL2META(unwrap(Value_val(Field(Arguments, 2)))));
  unsigned int LineNo = Unsigned_int_val(Field(Arguments, 3));
  auto *Ty = cast<DIType>(VAL2META(unwrap(Value_val(Field(Arguments, 4)))));

  return wrap(MetadataAsValue::get(
      Module.getContext(),
      DIB.createAutoVariable(Scope, Name, File, LineNo, Ty)));
}

/* llmodule -> llvalue */
CAMLprim LLVMValueRef extra_get_dbg_declare(LLVMModuleRef M) {
  return wrap(Intrinsic::getDeclaration(unwrap(M), Intrinsic::dbg_declare));
}

/* llmodule -> llvalue */
CAMLprim LLVMValueRef extra_empty_diexpression(LLVMModuleRef M) {
  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);
  return wrap(
      MetadataAsValue::get(Module.getContext(), DIB.createExpression()));
}
}
