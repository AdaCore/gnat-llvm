function Simple_Record (A : Integer; B : Integer) return Integer is

   type Test_Rec is record
      A, B : Integer;
   end record;

   R : Test_Rec;
begin
   R.A := A;
   R.B := B;
   return R.A + R.B;
end Simple_Record;
