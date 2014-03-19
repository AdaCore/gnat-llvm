package body Discr_Dyn is

   function Foo return Natural is
      type Int_Array is array (Natural range <>) of Integer;

      type A (I : Natural) is record
         C : Int_Array (1 .. I);
         A, B : Natural;
      end record;

      N : Natural := 12;
      A_Inst : A (N);
   begin
      A_Inst.C (5) := 8484;
      return A_Inst.C (5);
   end;

end Discr_Dyn;
