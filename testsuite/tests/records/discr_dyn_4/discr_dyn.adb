package body Discr_Dyn is

   function Foo return Natural is
      type Int_Array is array (Natural range <>) of Integer;

      type Discr_Dyn_A (I : Natural) is record
         D : Int_Array (1 .. I);
         A, B : Natural;
      end record;

      N : Natural := 12;
      A_Inst : Discr_Dyn_A (N);
   begin
      A_Inst.B := 1243;
      return A_Inst.B;
   end;

end Discr_Dyn;
