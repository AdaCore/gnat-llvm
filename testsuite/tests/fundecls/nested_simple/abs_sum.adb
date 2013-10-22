function Abs_Sum (I, J : Integer) return Integer is

   function Absolute (I : Integer) return Integer;

   --------------
   -- Absolute --
   --------------

   function Absolute (I : Integer) return Integer is
   begin
      if I < 0 then
         return -I;
      else
         return I;
      end if;
   end Absolute;

begin
   return Absolute (I) + Absolute (J);
end Abs_Sum;
