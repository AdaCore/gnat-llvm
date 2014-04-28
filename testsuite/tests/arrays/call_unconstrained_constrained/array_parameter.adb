package body Array_Parameter is
   type A is array (Integer range <>) of Integer;
   subtype A_Couple is A (1 .. 2);

   function F (Ar : A_Couple) return Integer is
   begin
      return Ar (1) + Ar (2);
   end F;

   function T (N : Integer) return Integer is
      Ar : A (1 .. N);
   begin
      Ar (1) := 12;
      Ar (2) := 14;
      if N = 2 then
         return F (Ar);
      else
         return 0;
      end if;
   end T;

end Array_Parameter;
