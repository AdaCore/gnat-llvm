function Opposite (I : Integer) return Integer is

   package Helper is
      procedure Opposite (I : in out Integer);
   end Helper;

   package body Helper is
      procedure Opposite (I : in out Integer) is
      begin
         I := -I;
      end Opposite;
   end Helper;

   Result : Integer := I;
begin
   Helper.Opposite (Result);
   return Result;
end Opposite;
