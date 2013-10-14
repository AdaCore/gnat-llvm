package body Access_Param_Record is

   type Test_Rec is record
      A, B : aliased Integer;
   end record;
   type Int_Access is access all Integer;

   procedure T (T : Int_Access) is
   begin
      T.all := T.all + 1;
   end T;

   function F return Integer is
      R : Test_Rec;
   begin
      R.A := 12;
      R.B := 16;
      T (R.A'Unchecked_Access);
      return R.A;
   end F;

end Access_Param_Record;
