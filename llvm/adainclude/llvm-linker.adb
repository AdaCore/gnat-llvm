pragma Ada_2005;
pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Linker is

   function Link_Modules2
     (Dest : LLVM.Types.Module_T;
      Src  : LLVM.Types.Module_T)
      return Boolean
   is
   begin
      return Link_Modules2_C (Dest, Src) /= 0;
   end Link_Modules2;

end LLVM.Linker;
