#include "llvm/IR/Attributes.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Metadata.h"
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
DIBuilder *
Create_Debug_Builder (Module *TheModule)
{
  TheModule->addModuleFlag(Module::Warning, "Debug Info Version",
			   DEBUG_METADATA_VERSION);
  TheModule->addModuleFlag(Module::Warning, "Dwarf Version", 4);
  return new DIBuilder (*TheModule);
}

extern "C"
DIFile *
Create_Debug_File_C (DIBuilder *DBld, const char *name, const char *dir)
{
  return  DBld->createFile (name, dir);
}

extern "C"
DICompileUnit *
Create_Debug_Compile_Unit (DIBuilder *DBld, DIFile *file)
{
  return DBld->createCompileUnit (dwarf::DW_LANG_Ada95, file,
				  "GNAT/LLVM", true, "", 0);
}

extern "C"
DISubprogram *
Create_Debug_Subprogram_C (DIBuilder *DBld, Function *func, DIFile *file,
			   const char *name, const char *ExtName, int lineno)
{
  SmallVector<Metadata *, 0> EltTy;
  DITypeRefArray TyArr = DBld->getOrCreateTypeArray (EltTy);
  DISubroutineType *st = DBld->createSubroutineType (TyArr);
  DISubprogram *subp = DBld->createFunction (file, name, ExtName, file,
					     lineno, st, false, true, lineno);
  func->setSubprogram (subp);
  return subp;
}

extern "C"
void
Finalize_Debug_Info (DIBuilder *DBld)
{
  DBld->finalize ();
}

extern "C"
DILexicalBlock *
Create_Debug_Lexical_Block (DIBuilder *DBld, DIScope *scope, DIFile *file,
			    int line, int col)
{
  return DBld->createLexicalBlock (scope, file, line, col);
}

extern "C"
void
Set_Debug_Loc (IRBuilder<> *bld, DISubprogram *subp, int line, int col)
{
  bld->SetCurrentDebugLocation (DebugLoc::get (line, col, subp));
}

extern "C"
MDBuilder *
Create_MDBuilder_In_Context (LLVMContext &Ctx)
{
    return new MDBuilder (Ctx);
}

extern "C"
MDNode *
Create_TBAA_Root (MDBuilder *MDHelper)
{
  return MDHelper->createTBAARoot ("Ada TBAA");
}

extern "C"
void
Add_Nest_Attribute (Function *fn, unsigned idx)
{
    fn->addAttribute (idx, Attribute::Nest);
}

extern "C"
MDNode *
Create_TBAA_Scalar_Type_Node_C (MDBuilder *MDHelper, const char *name,
				MDNode *parent)
{
  return MDHelper->createTBAAScalarTypeNode (name, parent);
}

extern "C"
MDNode *
Create_TBAA_Access_Tag (MDBuilder *MDHelper, MDNode *BaseType,
			MDNode *AccessType, unsigned long long offset)
{
  return MDHelper->createTBAAStructTagNode (BaseType, AccessType, offset);
}

extern "C"
void
Dump_LLVM_Type_C (Type *type)
{
    type->dump ();
}

extern "C"
void
Set_Volatile (Instruction *inst)
{
  if (StoreInst *SI = dyn_cast<StoreInst> (inst))
    SI->setVolatile (true);
  else if (LoadInst *LI = dyn_cast<LoadInst> (inst))
    LI->setVolatile (true);
}

extern "C"
void
Add_TBAA_Access (Instruction *inst, MDNode *md)
{
  inst->setMetadata (LLVMContext::MD_tbaa, md);
}

extern "C"
void
Set_Alloca_Align (AllocaInst *inst, unsigned align)
{
    inst->setAlignment (align);
}

/* The LLVM C interface only provide single-index forms of extractvalue
   and insertvalue, so provide the multi-index forms here.  */

extern "C"
Value *
Build_Extract_Value_C (IRBuilder<> *bld, Value *aggr,
		       unsigned *IdxList, unsigned NumIdx, char *name)
{
  return bld->CreateExtractValue (aggr, makeArrayRef (IdxList, NumIdx), name);
}

extern "C"
Value *
Build_Insert_Value_C (IRBuilder<> *bld, Value *aggr, Value *elt,
		     unsigned *IdxList, unsigned NumIdx, char *name)
{
  return bld->CreateInsertValue (aggr, elt, makeArrayRef (IdxList, NumIdx),
				 name);
}

extern "C"
unsigned char
Does_Not_Throw (Function *fn)
{
    return fn->doesNotThrow () ? 1 : 0;
}

extern "C"
void
Set_Does_Not_Throw (Function *fn)
{
    return fn->setDoesNotThrow ();
}

extern "C"
void
Set_Does_Not_Return (Function *fn)
{
    return fn->setDoesNotReturn ();
}

extern "C"
int
LLVM_Init_Module (Module *TheModule, const char *Filename, const char *target)
{
  // Initialize the target registry etc.
  InitializeAllTargetInfos();
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmParsers();
  InitializeAllAsmPrinters();

  std::string TargetTriple =
    target == NULL ? sys::getDefaultTargetTriple() : target;

  TheModule->setTargetTriple(TargetTriple);
  TheModule->setSourceFileName(Filename);
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
LLVM_Write_Module (Module *TheModule, int object, char *Filename)
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
