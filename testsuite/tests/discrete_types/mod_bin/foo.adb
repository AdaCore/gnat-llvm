package body Foo is
   function Mul (A : T) return Natural is
   begin
      return Natural (A * 3);
   end Mul;
end Foo;
