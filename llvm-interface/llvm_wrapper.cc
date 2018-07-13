#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/DerivedTypes.h"
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
Add_Cold_Attribute (Function *fn)
{
    fn->addFnAttr(Attribute::Cold);
}

extern "C"
void
Add_Dereferenceable_Attribute (Function *fn, unsigned idx,
			       unsigned long long Bytes)
{
    fn->addDereferenceableParamAttr (idx, Bytes);
}

extern "C"
void
Add_Dereferenceable_Or_Null_Attribute (Function *fn, unsigned idx,
				       unsigned long long Bytes)
{
    fn->addDereferenceableOrNullParamAttr (idx, Bytes);
}

extern "C"
void
Add_Inline_Always_Attribute (Function *fn)
{
    fn->addFnAttr(Attribute::AlwaysInline);
}

extern "C"
void
Add_Inline_Hint_Attribute (Function *fn)
{
    fn->addFnAttr(Attribute::InlineHint);
}

extern "C"
void
Add_Inline_No_Attribute (Function *fn)
{
    fn->addFnAttr(Attribute::NoInline);
}

extern "C"
void
Add_Nest_Attribute (Function *fn, unsigned idx)
{
    fn->addParamAttr (idx, Attribute::Nest);
}

extern "C"
void
Add_Noalias_Attribute (Function *fn, unsigned idx)
{
    fn->addParamAttr (idx, Attribute::NoAlias);
}

extern "C"
void
Add_Nocapture_Attribute (Function *fn, unsigned idx)
{
    fn->addParamAttr (idx, Attribute::NoCapture);
}

extern "C"
void
Add_Non_Null_Attribute (Function *fn, unsigned idx)
{
    fn->addParamAttr (idx, Attribute::NonNull);
}

extern "C"
void
Add_Readonly_Attribute (Function *fn, unsigned idx)
{
    fn->addParamAttr (idx, Attribute::ReadOnly);
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
    // type->dump ();
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
void
Initialize_LLVM (void)
{
  // Initialize the target registry etc.  These functions appear to be
  // in LLVM.Target, but they reference static inline function, so they
  // can only be used from C, not Ada.

  InitializeAllTargetInfos();
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmParsers();
  InitializeAllAsmPrinters();
}

extern "C"
void
LLVM_Init_Module (Module *TheModule, const char *Filename,
		  TargetMachine *TheTargetMachine)
{
  TheModule->setSourceFileName(Filename);
  TheModule->setDataLayout(TheTargetMachine->createDataLayout());
}

extern "C"
Value *
Get_Float_From_Words_And_Exp (LLVMContext *Context, Type *T, int Exp,
			      unsigned NumWords, const uint64_t Words[])
{
  auto LongInt = APInt (NumWords * 64, makeArrayRef (Words, NumWords));
  auto Initial = APFloat (T->getFltSemantics (),
			  APInt::getNullValue(T->getPrimitiveSizeInBits()));
  Initial.convertFromAPInt(LongInt, false, APFloat::rmTowardZero);
  auto Result = scalbn (Initial, Exp, APFloat::rmTowardZero);
  return ConstantFP::get (*Context, Result);
}

extern "C"
Value *
Pred_FP (LLVMContext *Context, Type *T, Value *Val)
{
  // We want to compute the predecessor of Val, but the "next" function
  // can return unnormalized, so we have to multiply by one (adding zero
  // doesn't do it).
  auto apf = dyn_cast<ConstantFP>(Val)->getValueAPF ();
  auto one = APFloat (T->getFltSemantics (), 1);
  apf.next (true);
  apf.multiply(one, APFloat::rmTowardZero);
  return ConstantFP:: get (*Context, apf);
}

extern "C"
bool
Is_Layout_Identical (StructType *T1, StructType *T2)
{
  return T1->isLayoutIdentical(T2);
}

