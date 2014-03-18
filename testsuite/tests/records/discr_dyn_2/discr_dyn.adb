package body Discr_Dyn is

   type Int_Array is array (Natural range <>) of Integer;
   type A (I : Natural) is record
      C : Int_Array (1 .. I);
      A, B : Natural;
   end record;

   function Get_I (A_Inst : access A) return Natural is
   begin
      return A_Inst.I;
   end Get_I;

   function Foo return Natural is
      A_Inst : aliased A (12);
   begin
      return Get_I (A_Inst'Access);
   end;

end Discr_Dyn;
