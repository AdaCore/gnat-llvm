procedure Foo (I : in out Integer)
is
   type Int_Access is access all Integer;

   Result_Var : Int_Access := I'Unrestricted_Access;
   R : Integer renames Result_Var.all;
begin
   R := R + 1;
end Foo;
