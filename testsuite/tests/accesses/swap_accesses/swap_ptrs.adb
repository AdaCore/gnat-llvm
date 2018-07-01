package body Swap_Ptrs is
   procedure Swap_Ptrs (A : out Int_Access; B : out Int_Access) is
      Tmp : Int_Access;
   begin
      Tmp := A;
      A := B;
      B := Tmp;
   end Swap_Ptrs;

   function Foo return Integer is
      I : aliased Integer := 42;
      J : aliased Integer := 84;
      A : Int_Access := I'Unchecked_Access;
      B : Int_Access := J'Unchecked_Access;
   begin
      Swap_Ptrs (A, B);
      return B.all;
   end Foo;

end Swap_Ptrs;
