package Foo is

   type Int_Array is array (Natural range <>) of Integer;
   procedure Bar (A : in out Int_Array);
   function Bal (A : in out Int_Array) return Integer;
   function Baz return Integer;

end Foo;
