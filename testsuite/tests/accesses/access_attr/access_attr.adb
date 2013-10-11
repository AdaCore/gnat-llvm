procedure Access_Attr (A : aliased in out Integer) is
   type Int_Access is access all Integer;
   B : Int_Access := A'Access;
begin
   B.all := B.all * 2;
end Access_Attr;
