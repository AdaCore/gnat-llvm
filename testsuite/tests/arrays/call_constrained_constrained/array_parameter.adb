package body Array_Parameter is
   type A is array (Integer range 1 .. 2) of Integer;

   function F (Ar : A) return Integer is
   begin
      return Ar (1) + Ar (2);
   end F;

   function T return Integer is
      Ar : A;
   begin
      Ar (1) := 12;
      Ar (2) := 14;
      return F (Ar);
   end T;

end Array_Parameter;
