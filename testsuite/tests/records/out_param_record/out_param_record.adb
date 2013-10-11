package body Out_Param_Record is

   type Test_Rec is record
      A, B : Integer;
   end record;

   procedure T (O : in out Integer) is
   begin
      O := O + 1;
   end T;

   function F return Integer is
      R : Test_Rec;
   begin
      R.A := 12;
      R.B := 16;
      T (R.A);
      return R.A;
   end F;

end Out_Param_Record;
