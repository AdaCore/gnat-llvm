function Foo (A, B : Integer) return Integer is

   function Foo1 return Integer is
   begin
      return A + B;
   end Foo1;

   function Foo2 (I : Integer) return Integer is

      function Foo3 (X : Integer) return Integer is
      begin
         return X + I + A - Foo1;
      end Foo3;

   begin
      return Foo3 (A + B - I);
   end Foo2;

begin
   return Foo2 (A + B);
end Foo;
