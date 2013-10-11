function Nested_Record return Integer is

   type Test_Rec is record
      A, B : Integer;
   end record;

   type Test_Rec_2 is record
      C : Integer;
      T : Test_Rec;
   end record;

   RR : Test_Rec;
   R : Test_Rec_2;
begin
   RR.A := 12;
   RR.B := 15;
   R.T := RR;
   return R.T.A;
end Nested_Record;
