package body Out_Param_Record is

   type Test_Rec is record
      A, B : Integer;
   end record;

   procedure T (T : in out Test_Rec) is
   begin
      T.A := T.A + 1;
      T.B := T.B + 1;
   end T;

   function F return Integer is
      R : Test_Rec;
   begin
      R.A := 12;
      R.B := 16;
      T (R);
      return R.A;
   end F;

end Out_Param_Record;
