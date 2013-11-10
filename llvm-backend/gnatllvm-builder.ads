with LLVM.Core; use LLVM.Core;
with Types; use Types;

package GNATLLVM.Builder is

   type Value_Array is array (Nat range <>) of Value_T;

   type Builder is new LLVM.Core.Base_Builder_T with null record;

   procedure Store (Bld : Builder; Expr : Value_T; Ptr : Value_T);
   --  Helper for LLVM's Build_Store

   function GEP
     (Bld : Builder; Ptr : Value_T; Indices : Value_Array; Name : String)
      return Value_T;
   --  Helper for LLVM's Build_GEP

end GNATLLVM.Builder;
