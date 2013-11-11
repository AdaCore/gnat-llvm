package Foo is

   type Int_Array is array (Natural range <>) of Integer;
   function Bar (A : Int_Array) return Integer;
   function Baz return Integer;

end Foo;
