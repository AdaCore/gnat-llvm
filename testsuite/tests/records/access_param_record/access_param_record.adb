package body Access_Param_Record is

   type Test_Rec is record
      A, B : Integer;
   end record;
   type Test_Rec_Access is access all Test_Rec;

   procedure T (T : Test_Rec_Access) is
   begin
      T.A := T.A + 1;
      T.B := T.B + 1;
   end T;

   function F return Integer is
      R : aliased Test_Rec;
   begin
      R.A := 12;
      R.B := 16;
      T (R'Unchecked_Access);
      return R.A;
   end F;

end Access_Param_Record;
