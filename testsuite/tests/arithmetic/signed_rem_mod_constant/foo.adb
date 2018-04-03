function Foo
  (Mod_Rem: Boolean; Neg_Pos: Boolean; A : Integer) return Integer
is
begin
   if Mod_Rem and then Neg_Pos then
      return A mod (-5);
   elsif Mod_Rem and then not Neg_Pos then
      return A mod 5;
   elsif not Mod_Rem and then Neg_Pos then
      return A rem (-5);
   else
      return A rem 5;
   end if;
end Foo;
