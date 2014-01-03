package Swap_Ptrs is
   type Int_Access is access all Integer;
   procedure Swap_Ptrs (A : out Int_Access; B : out Int_Access);
   function Foo return Integer;
end Swap_Ptrs;
