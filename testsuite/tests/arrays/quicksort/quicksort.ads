package QuickSort is
   type Int_Array is array (Natural range <>) of Integer;
   procedure Sort (Item : in out Int_Array);
   procedure Swap(Left, Right : in out Integer);
end QuickSort;
