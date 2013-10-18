package Foo is

   pragma Warnings (Off);
   type T is mod 2 ** 6;
   pragma Warnings (On);

   function Mul (A : T) return Natural;
end Foo;
