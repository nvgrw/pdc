
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
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"

//#define Builder_val(v) (*(LLVMBuilderRef *)(Data_custom_val(v)))
#define Value_val(v) ((LLVMValueRef)Op_val(v))
#define Metadata_val(v) ((LLVMMetadataRef)Op_val(v))
//#define VAL2META(V) (((MetadataAsValue *)V)->getMetadata())

using namespace llvm;

extern "C" {

/* llmodule -> difile_args -> llmetadata */
CAMLprim LLVMMetadataRef extra_difile(LLVMModuleRef M, value Arguments) {
  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);

  // Extract Arguments
  const char *Basename = String_val(Field(Arguments, 0));
  const char *Directory = String_val(Field(Arguments, 1));

  return wrap(DIB.createFile(Basename, Directory));
}

/* llmodule -> disubprogram_args -> llmetadata */
CAMLprim LLVMMetadataRef extra_disubprogram(LLVMModuleRef M, value Arguments) {
  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);

  // Extract Arguments
  auto *Scope = cast<DIScope>(unwrap(Metadata_val(Field(Arguments, 0))));
  const char *Name = String_val(Field(Arguments, 1));
  const char *LinkageName = String_val(Field(Arguments, 2));
  auto *File = cast<DIFile>(unwrap(Metadata_val(Field(Arguments, 3))));
  unsigned int LineNo = Unsigned_int_val(Field(Arguments, 4));
  auto *Ty = cast<DISubroutineType>(unwrap(Metadata_val(Field(Arguments, 5))));
  unsigned int ScopeLine = Unsigned_int_val(Field(Arguments, 6));

  DISubprogram *SP = DISubprogram::getDistinct(
      Module.getContext(), Scope, Name, LinkageName, File, LineNo, Ty,
      ScopeLine, nullptr, 0, 0, DINode::DIFlags::FlagZero,
      DISubprogram::DISPFlags::SPFlagDefinition, nullptr);
  return wrap(SP);
}

/* llmodule -> dilexicalblock_args -> llmetadata */
CAMLprim LLVMMetadataRef extra_dilexicalblock(LLVMModuleRef M,
                                              value Arguments) {
  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);

  // Extract Args
  auto *Scope = cast<DIScope>(unwrap(Metadata_val(Field(Arguments, 0))));
  auto *File = cast<DIFile>(unwrap(Metadata_val(Field(Arguments, 1))));
  unsigned int Line = Unsigned_int_val(Field(Arguments, 2));
  unsigned int Col = Unsigned_int_val(Field(Arguments, 3));

  return wrap(DIB.createLexicalBlock(Scope, File, Line, Col));
}

/* llmodule -> dilocalvariable_args -> llmetadata */
CAMLprim LLVMMetadataRef extra_dilocalvariable(LLVMModuleRef M,
                                               value Arguments) {
  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);

  // Extract Args
  auto *Scope = cast<DIScope>(unwrap(Metadata_val(Field(Arguments, 0))));
  const char *Name = String_val(Field(Arguments, 1));
  auto *File = cast<DIFile>(unwrap(Metadata_val(Field(Arguments, 2))));
  unsigned int LineNo = Unsigned_int_val(Field(Arguments, 3));
  auto *Ty = cast<DIType>(unwrap(Metadata_val(Field(Arguments, 4))));

  return wrap(DIB.createAutoVariable(Scope, Name, File, LineNo, Ty));
}

/* llmodule -> llvalue */
CAMLprim LLVMValueRef extra_get_dbg_declare(LLVMModuleRef M) {
  return wrap(Intrinsic::getDeclaration(unwrap(M), Intrinsic::dbg_declare));
}

/* llmodule -> llmetadata */
CAMLprim LLVMMetadataRef extra_empty_diexpression(LLVMModuleRef M) {
  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);
  return wrap(DIB.createExpression());
}

/* llmodule -> string -> llmetadata */
CAMLprim LLVMMetadataRef extra_unspecified_ditype(LLVMModuleRef M, value Name) {
  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);
  return wrap(DIB.createUnspecifiedType(String_val(Name)));
}

/* llcontext -> llmetadata -> llvalue */
CAMLprim LLVMValueRef extra_metadata_to_value(LLVMContextRef C,
                                              LLVMMetadataRef M) {
  return wrap(MetadataAsValue::get(*unwrap(C), unwrap(M)));
}

/* llvalue -> llmetadata */
CAMLprim LLVMMetadataRef extra_value_to_metadata(LLVMValueRef V) {
  return wrap(ValueAsMetadata::get(unwrap(V)));
}

/* unit -> llmetadata */
CAMLprim LLVMMetadataRef extra_mdnull(value Unit) { return nullptr; }

/* llmodule -> llmetadata array -> llmetadata */
CAMLprim LLVMMetadataRef extra_disubroutine_type(LLVMModuleRef M,
                                                 value TypeVals) {
  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);

  unsigned Length = Wosize_val(TypeVals);
  DITypeRefArray ParameterTypes = DIB.getOrCreateTypeArray(ArrayRef<Metadata *>(
      unwrap((LLVMMetadataRef *)Op_val(TypeVals)), Length));
  return wrap(DIB.createSubroutineType(ParameterTypes));
}
}
