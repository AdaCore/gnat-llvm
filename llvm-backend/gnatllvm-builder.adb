with LLVM.Core; use LLVM.Core;

package body GNATLLVM.Builder is

   ---------
   -- GEP --
   ---------

   function GEP
     (Bld : Builder; Ptr : Value_T; Indices : Value_Array; Name : String)
      return Value_T
   is
     (GEP (Bld, Ptr, Indices'Address, Indices'Length, Name));

   -----------
   -- Store --
   -----------

   procedure Store (Bld : Builder; Expr : Value_T; Ptr : Value_T)
   is
      Dummy : Value_T;
      pragma Unreferenced (Dummy);
   begin
      Dummy := Build_Store (Bld, Expr, Ptr);
   end Store;

end GNATLLVM.Builder;
