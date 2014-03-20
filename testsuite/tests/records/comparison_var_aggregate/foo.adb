function Foo (A, B, C, D : Integer; Equality : Boolean) return Boolean is
   type R_Type is record
      A, B : Integer;
   end record;

   R1 : constant R_Type := (A, B);
begin
   if Equality then
      return R1 = (C, D);
   else
      return R1 /= (C, D);
   end if;
end Foo;
