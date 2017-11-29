package body Pak is
   Var : Integer := 0;

   function Value return Integer is
   begin
      return Var;
   end Value;

begin
   Var := 42;
end Pak;
