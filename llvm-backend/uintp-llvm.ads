with LLVM.Core; use LLVM.Core;

package Uintp.LLVM is

   function UI_To_LLVM (T : Type_T; U : Uint) return Value_T;
   --  Convert a Uint into an LLVM native integer constant

end Uintp.LLVM;
