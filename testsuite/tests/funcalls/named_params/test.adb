package body Test is
   function Test return Natural is
      function Foo (A : Natural; B : Natural) return Natural is
      begin
         return A + B;
      end Foo;
      Bar : Natural := Foo (B => 16, A => 15);
   begin
      return Bar;
   end Test;
end Test;
