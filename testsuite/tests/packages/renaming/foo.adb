function Foo (I : Integer) return Integer is

   package P is
      A : Integer := I;
   end P;

   package PR renames P;

begin
   PR.A := PR.A + 1;
   return P.A;
end Foo;
