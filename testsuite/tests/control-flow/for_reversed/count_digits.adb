with Interfaces;

use type Interfaces.Unsigned_32;

function Count_Digits (X : Interfaces.Unsigned_32) return Natural is
   Digits_Set : constant
      array (Interfaces.Unsigned_32 range 0 .. 9)
      of Character := "0123456789";
   Image      : array (1 .. 10) of Character;
   Tmp        : Interfaces.Unsigned_32 := X;
begin
   for I in reverse Image'Range loop
      Image (I) := Digits_Set (Tmp mod 10);
      Tmp := Tmp / 10;
   end loop;

   for I in reverse Image'Range loop
      if Image (I) = '0' then
         if I = Image'Last then
            return 1;
         else
            return Image'Last - I + 1;
         end if;
      end if;
   end loop;

   return Image'Length;
end Count_Digits;
