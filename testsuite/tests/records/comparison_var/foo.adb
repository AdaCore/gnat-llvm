function Foo (A, B, C, D : Integer; Equality : Boolean) return Boolean is
   type R_Type is record
      A, B : Integer;
   end record;

   R1 : constant R_Type := (A, B);
   R2 : constant R_Type := (C, D);
begin
   if Equality then
      return R1 = R2;
   else
      return R1 /= R2;
   end if;
end Foo;
