with LLVM.Core;

package GNATLLVM.Environment is

   --  Expansed Ada-to-LLVM translation context: gathers global information

   type Environ is tagged record
      Ctx : LLVM.Core.Context_T;
      Bld : LLVM.Core.Builder_T;
      Mdl : LLVM.Core.Module_T;
      --  Pure-LLVM environment: LLVM context, instruction builder and current
      --  module.
   end record;

end GNATLLVM.Environment;
