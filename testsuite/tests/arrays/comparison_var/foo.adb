function Foo (A, B, C, D : Integer; Equality : Boolean) return Boolean is
   type A_Type is array (Natural range <>) of Integer;

   A1 : constant A_Type := (1 .. A => B);
   A2 : constant A_Type := (1 .. C => D);
begin
   if Equality then
      return A1 = A2;
   else
      return A1 /= A2;
   end if;
end Foo;
