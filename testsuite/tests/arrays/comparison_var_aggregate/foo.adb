function Foo (A, B, C, D : Integer; Equality : Boolean) return Boolean is
   type A_Type is array (Natural range <>) of Integer;

   A1 : constant A_Type := (1 .. A => B);
begin
   if Equality then
      return A1 = (1 .. C => D);
   else
      return A1 /= (1 .. C => D);
   end if;
end Foo;
