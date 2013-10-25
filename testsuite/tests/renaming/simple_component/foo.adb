procedure Foo (I : in out Integer)
is
   type Rec is record
      I : Integer;
      J : Integer;
   end record;

   R : Rec := (0, 1);
   R_I : Integer renames R.I;
   R_J : Integer renames R.J;
begin
   R_I := I;
   I := R_I + R_J;
end Foo;
