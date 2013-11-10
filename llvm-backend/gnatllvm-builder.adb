package body GNATLLVM.Builder is

   ---------
   -- GEP --
   ---------

   function GEP
     (Bld : Builder; Ptr : Value_T; Indices : Value_Array; Name : String)
      return Value_T
   is
     (Bld.GEP (Ptr, Indices'Address, Indices'Length, Name));

   -----------
   -- Store --
   -----------

   procedure Store (Bld : Builder; Expr : Value_T; Ptr : Value_T)
   is
      Dummy : Value_T;
      pragma Unreferenced (Dummy);
   begin
      Dummy := Bld.Store (Expr, Ptr);
   end Store;

end GNATLLVM.Builder;
