with Einfo; use Einfo;
with Types; use Types;

with LLVM.Core; use LLVM.Core;

with GNATLLVM.Bounds; use GNATLLVM.Bounds;
with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.Utils; use GNATLLVM.Utils;

package GNATLLVM.Arrays is

   procedure Extract_Array_Info
     (Env         : Environ;
      Array_Node  : Node_Id;
      Array_Descr : out Value_T;
      Array_Type  : out Entity_Id);
   --  Set Array_Type to the type of Array_Node. If it is a constrained array,
   --  set Array_Descr to No_Value_T, or emit the value corresponding to
   --  Array_Node if it is unconstrained.

   function Array_Size
     (Env : Environ; Array_Type : Entity_Id;
      Containing_Record_Instance : Value_T := No_Value_T) return Value_T;

   function Array_Bound
     (Env         : Environ;
      Array_Descr : Value_T;
      Array_Type  : Entity_Id;
      Bound       : Bound_T;
      Dim         : Natural := 1) return Value_T;
   --  Compute the bound for the array corresponding to Array_Descr, whose type
   --  is Array_Type. If Array_Type is a constrained array, Array_Descr will
   --  not be used, and can thus then be No_Value_T. Otherwise, it will be
   --  used to compute the bound at runtime.

   function Array_Length
     (Env         : Environ;
      Array_Descr : Value_T;
      Array_Type  : Entity_Id) return Value_T;
   --  Emit code to compute the length for the array corresponding to
   --  Array_Descr, whose type is Array_Type. If Array_Type is a constrained
   --  array, Array_Descr will not be used, and can thus then be No_Value_T.
   --  Otherwise, it will be used to compute the length at runtime.

   function Array_Data
     (Env         : Environ;
      Array_Descr : Value_T;
      Array_Type  : Entity_Id) return Value_T;
   --  Emit code to compute the address of the array data and return the
   --  corresponding value. Handle both constrained and unconstrained arrays,
   --  depending on Array_Type. If this is a constrained array, Array_Descr
   --  must already be a pointer to the array data, otherwise, it must be a
   --  fat pointer.

   function Array_Fat_Pointer
     (Env        : Environ;
      Array_Data : Value_T;
      Array_Type : Entity_Id) return Value_T
     with Pre => Is_Constrained (Array_Type);
   --  Wrap a fat pointer around Array_Data according to its type Array_Type
   --  and return the created fat pointer.

end GNATLLVM.Arrays;
