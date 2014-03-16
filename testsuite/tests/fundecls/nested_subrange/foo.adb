function Foo (I, J : Integer) return Integer is

   A : Integer := I + 1;
   B : Integer := J - 1;
   subtype Bar is Integer range A .. B;

   function Diff return Integer is
   begin
      return Bar'Last - Bar'First;
   end Diff;

begin
   A := I;
   B := J;
   return Diff;
end Foo;
