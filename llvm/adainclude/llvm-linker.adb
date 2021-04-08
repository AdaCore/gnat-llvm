pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Linker is

   function Link_Modules_2
     (Dest : LLVM.Types.Module_T;
      Src  : LLVM.Types.Module_T)
      return Boolean
   is
   begin
      return Link_Modules_2_C (Dest, Src) /= 0;
   end Link_Modules_2;

end LLVM.Linker;
