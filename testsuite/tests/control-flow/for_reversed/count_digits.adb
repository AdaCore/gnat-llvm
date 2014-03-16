with Interfaces;

use type Interfaces.Unsigned_8;
use type Interfaces.Unsigned_32;

function Count_Digits (X : Interfaces.Unsigned_32) return Natural is
   Digits_Set : constant
      array (Interfaces.Unsigned_32 range 0 .. 9)
      of Character := "0123456789";
   Image      : array (Interfaces.Unsigned_8 range 1 .. 10) of Character;
   Tmp        : Interfaces.Unsigned_32 := X;

   Result     : Integer := 1;
begin
   for I in reverse Image'Range loop
      declare
         Next_Tmp : constant Interfaces.Unsigned_32 :=
            Tmp / 10;
         Current_Digit : constant Interfaces.Unsigned_32 :=
            Tmp - 10 * Next_Tmp;
      begin
         Image (I) := Digits_Set (Current_Digit);
         Tmp := Next_Tmp;
      end;

      if Image (I) /= '0' then
         Result := Integer (Image'Last - I) + 1;
      end if;
   end loop;

   return Integer'Max (1, Result);
end Count_Digits;
