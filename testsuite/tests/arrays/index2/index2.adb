function Index2 (A, B: Integer) return Integer is
   
   function Ident_Int (A: Integer) return Integer is
   begin
      return A;
   end Ident_Int;

   type XB is array (Integer range <>, Integer range <>) of Integer;
   subtype X is XB(Ident_Int(1) .. Ident_Int(2),
                   Ident_Int(1) .. Ident_Int (2));
   Xx: X := ((1, 2), (3, 4));

begin
   
   return XX (A, B);
end Index2;

