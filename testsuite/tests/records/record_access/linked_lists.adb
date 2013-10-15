package body Linked_Lists is

   function Length (L : List) return Natural is
      Cur    : List    := L;
      Result : Natural := 0;
   begin

      while Cur /= null loop
         Result := Result + 1;
         Cur    := Cur.Next;
      end loop;

      return Result;
   end Length;

end Linked_Lists;
