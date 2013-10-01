with LLVM.Core;

package Utils is
   type Context is record
      Ctx : LLVM.Core.Context_T;
      Bld : LLVM.Core.Builder_T;
      Mdl : LLVM.Core.Module_T;
   end record;
end Utils;
