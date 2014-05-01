with Types; use Types;

with LLVM.Core; use LLVM.Core;

with GNATLLVM.Environment; use GNATLLVM.Environment;

package GNATLLVM.Bounds is

   type Bound_T is (Low, High);

   function Bounds_To_Length
     (Env                   : Environ;
      Low_Bound, High_Bound : Value_T;
      Bounds_Type           : Entity_Id) return Value_T;
   --  Return the length of the Low_Bound .. High_Bound range, handling the
   --  empty case. Bounds_Type indicates how to interpret the provided bounds
   --  with respect to signedness.

end GNATLLVM.Bounds;
