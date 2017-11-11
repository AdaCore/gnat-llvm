pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;

package LLVM.Linker is

   type Linker_Mode_T is 
     (Linker_Destroy_Source,
      Linkerpreservesource_Removed);
   pragma Convention (C, Linker_Mode_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Linker.h:28

   function Link_Modules2 (Dest : LLVM.Types.Module_T; Src : LLVM.Types.Module_T) return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Linker.h:35
   pragma Import (C, Link_Modules2, "LLVMLinkModules2");

end LLVM.Linker;

