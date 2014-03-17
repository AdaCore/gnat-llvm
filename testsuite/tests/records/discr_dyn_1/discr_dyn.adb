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
      return A_Inst.I;
   end;

end Discr_Dyn;
