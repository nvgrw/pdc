
#include <vector>

#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/misc.h"

#include "llvm-c/Types.h"

#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"

#define Metadata_val(v) ((LLVMMetadataRef)Op_val(v))

using namespace llvm;

extern "C" {

/* llmodule -> DIFile.args -> llmetadata */
CAMLprim LLVMMetadataRef extra_difile(LLVMModuleRef M, value Arguments) {
  CAMLparam1(Arguments);

  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);

  // Extract Arguments
  const char *Basename = String_val(Field(Arguments, 0));
  const char *Directory = String_val(Field(Arguments, 1));

  CAMLreturnT(LLVMMetadataRef, wrap(DIB.createFile(Basename, Directory)));
}

/* llmodule -> DISubprogram.args -> llmetadata */
CAMLprim LLVMMetadataRef extra_disubprogram(LLVMModuleRef M, value Arguments) {
  CAMLparam1(Arguments);

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
  auto *Unit = cast<DICompileUnit>(unwrap(Metadata_val(Field(Arguments, 7))));

  DISubprogram *SP = DISubprogram::getDistinct(
      Module.getContext(), Scope, Name, LinkageName, File, LineNo, Ty,
      ScopeLine, nullptr, 0, 0, DINode::DIFlags::FlagZero,
      DISubprogram::DISPFlags::SPFlagDefinition, Unit);
  CAMLreturnT(LLVMMetadataRef, wrap(SP));
}

/* llmodule -> DILexicalBlock.args -> llmetadata */
CAMLprim LLVMMetadataRef extra_dilexicalblock(LLVMModuleRef M,
                                              value Arguments) {
  CAMLparam1(Arguments);

  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);

  // Extract Args
  auto *Scope = cast<DIScope>(unwrap(Metadata_val(Field(Arguments, 0))));
  auto *File = cast<DIFile>(unwrap(Metadata_val(Field(Arguments, 1))));
  unsigned int Line = Unsigned_int_val(Field(Arguments, 2));
  unsigned int Col = Unsigned_int_val(Field(Arguments, 3));

  CAMLreturnT(LLVMMetadataRef,
              wrap(DIB.createLexicalBlock(Scope, File, Line, Col)));
}

/* llmodule -> DILocalVariable.args -> llmetadata */
CAMLprim LLVMMetadataRef extra_dilocalvariable(LLVMModuleRef M,
                                               value Arguments) {
  CAMLparam1(Arguments);

  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);

  // Extract Args
  auto *Scope = cast<DIScope>(unwrap(Metadata_val(Field(Arguments, 0))));
  const char *Name = String_val(Field(Arguments, 1));
  auto *File = cast<DIFile>(unwrap(Metadata_val(Field(Arguments, 2))));
  unsigned int LineNo = Unsigned_int_val(Field(Arguments, 3));
  auto *Ty = cast<DIType>(unwrap(Metadata_val(Field(Arguments, 4))));

  CAMLreturnT(LLVMMetadataRef,
              wrap(DIB.createAutoVariable(Scope, Name, File, LineNo, Ty)));
}

/* llmodule -> DICompileUnit.args -> llmetadata */
CAMLprim LLVMMetadataRef extra_dicompileunit(LLVMModuleRef M, value Arguments) {
  CAMLparam1(Arguments);

  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);

  // Extract Args
  auto *File = cast<DIFile>(unwrap(Metadata_val(Field(Arguments, 0))));
  const char *Producer = String_val(Field(Arguments, 1));
  bool isOptimized = Bool_val(Field(Arguments, 2));

  CAMLreturnT(
      LLVMMetadataRef,
      wrap(DIB.createCompileUnit(
          dwarf::
              DW_LANG_C /* we're mostly piggybacking off of C's conventions */,
          File, Producer, isOptimized, "", 0)));
}

/* llmodule -> llvalue */
CAMLprim LLVMValueRef extra_get_dbg_declare(LLVMModuleRef M) {
  return wrap(Intrinsic::getDeclaration(unwrap(M), Intrinsic::dbg_declare));
}

/* llmodule -> llvalue */
CAMLprim LLVMValueRef extra_get_dbg_value(LLVMModuleRef M) {
  return wrap(Intrinsic::getDeclaration(unwrap(M), Intrinsic::dbg_value));
}

/* llmodule -> llmetadata */
CAMLprim LLVMMetadataRef extra_empty_diexpression(LLVMModuleRef M) {
  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);
  return wrap(DIB.createExpression());
}

/* llmodule -> int array -> llmetadata */
CAMLprim LLVMMetadataRef extra_diexpression(LLVMModuleRef M, value Opcodes) {
  CAMLparam1(Opcodes);
  unsigned Size = Wosize_val(Opcodes);

  uint64_t *Ops = new uint64_t[Size];
  for (unsigned I = 0; I < Size; I++) {
    Ops[I] = (int64_t)Unsigned_int_val(Field(Opcodes, I));
  }

  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);
  auto *Expression = DIB.createExpression(ArrayRef<uint64_t>(Ops, Size));
  // DIExpression copies Ops
  delete[] Ops;

  CAMLreturnT(LLVMMetadataRef, wrap(Expression));
}

/* llmodule -> string -> llmetadata */
CAMLprim LLVMMetadataRef extra_unspecified_ditype(LLVMModuleRef M, value Name) {
  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);
  return wrap(DIB.createUnspecifiedType(String_val(Name)));
}

/* llmodule -> string -> int -> int -> llmetadata */
CAMLprim LLVMMetadataRef extra_basic_ditype(LLVMModuleRef M, value Name,
                                            value SizeInBits, value Encoding) {
  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);
  return wrap(DIB.createBasicType(String_val(Name), Int_val(SizeInBits),
                                  Unsigned_int_val(Encoding)));
}

/* llmodule -> int -> llmetadata -> llmetadata array -> llmetadata */
CAMLprim LLVMMetadataRef extra_array_ditype(LLVMModuleRef M, value Size,
                                            LLVMMetadataRef Ty, value S) {
  CAMLparam1(S);

  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);

  unsigned Length = Wosize_val(S);
  DINodeArray Subscripts = DIB.getOrCreateArray(
      ArrayRef<Metadata *>(unwrap((LLVMMetadataRef *)Op_val(S)), Length));
  CAMLreturnT(LLVMMetadataRef,
              wrap(DIB.createArrayType(Unsigned_int_val(Size), 0,
                                       cast<DIType>(unwrap(Metadata_val(Ty))),
                                       Subscripts)));
}

/* llmodule -> int -> int -> llmetadata */
CAMLprim LLVMMetadataRef extra_disubrange(LLVMModuleRef M, value Lo,
                                          value Count) {
  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);
  return wrap(DIB.getOrCreateSubrange(Int_val(Lo), Int_val(Count)));
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
  CAMLparam1(TypeVals);

  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);

  unsigned Length = Wosize_val(TypeVals);
  DITypeRefArray ParameterTypes = DIB.getOrCreateTypeArray(ArrayRef<Metadata *>(
      unwrap((LLVMMetadataRef *)Op_val(TypeVals)), Length));
  CAMLreturnT(LLVMMetadataRef, wrap(DIB.createSubroutineType(ParameterTypes)));
}

/* llmodule -> llvalue -> AttachInstLocation.args -> unit */
CAMLprim value extra_attach_inst_location(LLVMModuleRef M, LLVMValueRef Invoke,
                                          value Arguments) {
  CAMLparam1(Arguments);

  Module &Module = *unwrap(M);
  DIBuilder DIB(Module);

  assert(isa<Instruction>(unwrap(Invoke)) && "Value needs to be instruction");

  // Extract Args
  auto Line = Unsigned_int_val(Field(Arguments, 0));
  auto Col = Unsigned_int_val(Field(Arguments, 1));
  auto *Scope = cast<DIScope>(unwrap(Metadata_val(Field(Arguments, 2))));

  auto *I = cast<Instruction>(unwrap(Invoke));
  I->setDebugLoc(DebugLoc::get(Line, Col, Scope));

  CAMLreturn(Val_unit);
}

/* llmodule -> string -> int -> unit */
CAMLprim value extra_add_int_module_flag(LLVMModuleRef M, value Key,
                                         value Val) {
  llvm::Module *Module = unwrap(M);
  Module->addModuleFlag(Module::Warning, String_val(Key),
                        Unsigned_int_val(Val));
  return Val_unit;
}
}
