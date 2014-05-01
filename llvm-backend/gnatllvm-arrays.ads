with Types; use Types;

with LLVM.Core; use LLVM.Core;

with GNATLLVM.Bounds; use GNATLLVM.Bounds;
with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.Utils; use GNATLLVM.Utils;

package GNATLLVM.Arrays is

   function Array_Size
     (Env : Environ; Array_Type : Entity_Id;
      Containing_Record_Instance : Value_T := No_Value_T) return Value_T;

   function Array_Length (Env : Environ; Array_Node : Node_Id) return Value_T;
   --  Emit code to compute the length for the array corresponding to
   --  Array_Node and return the corresponding value.

   function Array_Bound
     (Env : Environ; Array_Node : Node_Id;
      Bound : Bound_T; Dim : Natural := 1) return Value_T;
   --  Compute the bound for the array corresponding to Array_Node. Depending
   --  on whether the array is constrained or not, this will compute the bound
   --  statically or at runtime.

   function Array_Bound_Addr
     (Env : Environ; Array_Ptr : Value_T;
      Bound : Bound_T; Dim : Natural) return Value_T;
   --  Compute the bound for the array corresponding to Array_Ptr. The pointer
   --  must be a fat pointer (i.e. containing the bounds of the array).

   function Array_Bound
     (Env : Environ; Array_Ptr : Value_T;
      Bound : Bound_T; Dim : Natural) return Value_T;
   --  Wrapper around Array_Bound_Addr that returns the value of the bound,
   --  instead of the address of element at the bound.

end GNATLLVM.Arrays;
