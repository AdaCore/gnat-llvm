procedure Foo (I : in out Integer)
is
   type Couple is array (1 .. 2) of Integer;

   R : Couple := (0, 1);
   R_First : Integer renames R (1);
   R_Second : Integer renames R (2);
begin
   R_First := I;
   I := R_First + R_Second;
end Foo;
