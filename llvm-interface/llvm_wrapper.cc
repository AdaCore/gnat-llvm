#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

using namespace llvm;
using namespace llvm::sys;

extern "C"
int
LLVM_Init_Module (Module *TheModule)
{
  // Initialize the target registry etc.
  InitializeAllTargetInfos();
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmParsers();
  InitializeAllAsmPrinters();

  auto TargetTriple = sys::getDefaultTargetTriple();
  TheModule->setTargetTriple(TargetTriple);

  std::string Error;
  auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

  // Return an error if we couldn't find the requested target.
  // This generally occurs if we've forgotten to initialise the
  // TargetRegistry or we have a bogus target triple.
  if (!Target) {
    return 1;
  }

  auto CPU = "generic";
  auto Features = "";

  TargetOptions opt;
  auto RM = Optional<Reloc::Model>();
  auto TheTargetMachine =
    Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);

  TheModule->setDataLayout(TheTargetMachine->createDataLayout());
  return 0;
}

extern "C"
int
LLVM_Write_Object (Module *TheModule, int object, char *Filename)
{
  std::error_code EC;
  raw_fd_ostream dest(Filename, EC, sys::fs::F_None);

  if (EC) {
    // Could not open the file
    return 2;
  }

  legacy::PassManager pass;
  auto FileType = object ?
    TargetMachine::CGFT_ObjectFile : TargetMachine::CGFT_AssemblyFile;

  auto TargetTriple = TheModule->getTargetTriple();

  std::string Error;
  auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);
  auto CPU = "generic";
  auto Features = "";

  TargetOptions opt;
  auto RM = Optional<Reloc::Model>();
  auto TheTargetMachine =
      Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);

  if (TheTargetMachine->addPassesToEmitFile(pass, dest, FileType)) {
    // Could not emit a file of this type
    return 3;
  }

  pass.run(*TheModule);
  dest.flush();

  return 0;
}
