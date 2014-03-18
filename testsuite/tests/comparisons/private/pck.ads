package Pck is

   type T is private;

   function Zero return T;

private

   type T is new Integer;

end Pck;
