package body Pak is
   function Double (I : Integer) return Integer is
   begin
      return I * 2;
   end Double;

   function Quadruple (I : Integer) return Integer is
   begin
      return Double (I) * 2;
   end Quadruple;
end Pak;
