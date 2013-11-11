package body QuickSort is

   procedure Swap(Left, Right : in out Integer) is
      Temp : Integer := Left;
   begin
      Left := Right;
      Right := Temp;
   end Swap;
 
   procedure Sort (Item : in out Int_Array) is
 
      Pivot_Index : Natural;
      Pivot_Value : Integer;
      Right       : Natural := Item'Last;
      Left        : Natural := Item'First;
 
   begin
      if Item'Length > 1 then
         Pivot_Index := Item'First;
         Pivot_Value := Item(Pivot_Index);

         Left  := Item'First;
         Right := Item'Last;

         loop
            while Left < Item'Last and then Item(Left) < Pivot_Value loop
               Left := Left + 1;
            end loop;
            while Right > Item'First and then Item(Right) > Pivot_Value loop
               Right := Right - 1;
            end loop;

            exit when Left >= Right;

            Swap (Item (Left), Item (Right));
            if Left < Item'Last and then Right > Item'First then
               Left := Left + 1;
               Right := Right - 1;
            end if;
         end loop;

         if Right > Item'First then
            Sort (Item (Item'First .. Right));
         end if;
         if Left < Item'Last then
            Sort (Item (Right + 1 .. Item'Last));
         end if;
      end if;
   end Sort;

end QuickSort;
