#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/PassSupport.h"
#include "llvm/CodeGen/ChooseLowering.h"
#include "llvm/InitializePasses.h"

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

using namespace llvm;

namespace {
struct A4RRandomProvider : public FunctionPass {
  static char ID;

  A4RRandomProvider() : FunctionPass(ID) {
    initializeCodeGen(*PassRegistry::getPassRegistry());
  }
  StringRef getPassName() const override;
  void getAnalysisUsage(AnalysisUsage &AU) const override;

  bool doInitialization(Module &M) override;
  bool runOnFunction(Function &F) override;
};
} // anonymous namespace

char A4RRandomProvider::ID = 0;

StringRef A4RRandomProvider::getPassName() const {
  return "HELLO HELLO HELLO HELLO HELLO";
}

void A4RRandomProvider::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<ChooseLowering>();
}

bool A4RRandomProvider::doInitialization(Module &M) {
  return false;
}

bool A4RRandomProvider::runOnFunction(Function &F) {
  errs() << "Hello: ";
  errs().write_escaped(F.getName()) << '\n';
  return false;
}

static llvm::RegisterStandardPasses Y(
    llvm::PassManagerBuilder::EP_EarlyAsPossible,
    [](const llvm::PassManagerBuilder &Builder,
       llvm::legacy::PassManagerBase &PM) { PM.add(new A4RRandomProvider()); });