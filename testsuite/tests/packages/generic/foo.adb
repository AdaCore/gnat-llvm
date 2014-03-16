function Foo (I : Integer) return Integer is

   generic
      I : Integer;
   package P is
      function Get return Integer;
   end P;

   package body P is
      function Get return Integer is
      begin
         return I;
      end Get;
   end P;

   package PR is new P (I => I);

begin
   return PR.Get;
end Foo;
