
#include "llvm/IR/PassManager.h"

using namespace llvm;

namespace {
struct A4RandomProviderPass : public PassInfoMixin<A4RandomProviderPass> {
};
}