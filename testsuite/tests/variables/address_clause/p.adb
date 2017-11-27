package body P is
   Global : aliased Integer := 0;

   function Test_Address (X : Integer) return Integer is
      Var : aliased Integer;
      for Var'Address use Global'Address;
   begin
      Var := Var + X;
      return Global;
   end Test_Address;

end P;
