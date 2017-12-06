package body Pak is
   function Func return Integer is
   begin
      return 11;
   end Func;

   Var : Integer := 1;
   Var2 : Integer := Func;

   function Value return Integer is
   begin
      return Var + Var2;
   end Value;

begin
   Var := Var + 30;
end Pak;
