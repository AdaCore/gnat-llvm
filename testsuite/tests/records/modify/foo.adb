function Foo (A : Integer; B : Integer) return Integer is

   type Test_Rec is record
      A, B : Integer;
   end record;

   R : Test_Rec := (A, B);
begin
   R := (R.A + R.B, 0);
   return R.A;
end Foo;
