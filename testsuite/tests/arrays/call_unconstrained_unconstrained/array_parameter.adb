package body Array_Parameter is
   type A is array (Integer range <>) of Integer;

   function F (Ar : A) return Integer is
   begin
      return Ar (1) + Ar (2);
   end F;

   function G (Ar : A) return Integer is
   begin
      return F (Ar);
   end G;

   function T (N : Integer) return Integer is
      Ar : A (1 .. N);
   begin
      Ar (1) := 12;
      Ar (2) := 14;
      return G (Ar);
   end T;

end Array_Parameter;
