with Interfaces.C; use Interfaces.C;

with Atree;  use Atree;
with Einfo;  use Einfo;
with Nlists; use Nlists;
with Sinfo;  use Sinfo;
with Uintp;  use Uintp;

with GNATLLVM.Compile; use GNATLLVM.Compile;
with GNATLLVM.Types;   use GNATLLVM.Types;

package body GNATLLVM.Arrays is

   -----------------
   -- Array_Bound --
   -----------------

   function Array_Bound
     (Env : Environ; Array_Ptr : Value_T;
      Bound : Bound_T; Dim : Natural) return Value_T
   is
   begin
      return Env.Bld.Load
        (Array_Bound_Addr (Env, Array_Ptr, Bound, Dim),
         (if Bound = Low
          then "load-low-bound"
          else "load-high-bound"));
   end Array_Bound;

   ----------------------
   -- Array_Bound_Addr --
   ----------------------

   function Array_Bound_Addr
     (Env : Environ; Array_Ptr : Value_T;
      Bound : Bound_T; Dim : Natural) return Value_T
   is
      Bounds_Ptr             : constant Value_T :=
        Env.Bld.Struct_GEP (Array_Ptr, 1, "gep-bounds-array");
      --  Get a pointer to the structure that contains array bounds

      Bounds_Pair_Idx        : constant Natural := (Dim - 1) * 2;
      --  In such a structure, bounds are stored as a sequence of (lower bound,
      --  upper bound) pairs : get the offset of such a pair.

      Bound_Idx              : constant unsigned :=
        unsigned (Bounds_Pair_Idx) + (if Bound = Low then 0 else 1);
   begin
      return Env.Bld.Struct_GEP
        (Bounds_Ptr,
         Bound_Idx,
         (if Bound = Low
          then "gep-low-bound"
          else "gep-high-bound"));
   end Array_Bound_Addr;

   -----------------
   -- Array_Bound --
   -----------------

   function Array_Bound
     (Env : Environ; Array_Node : Node_Id;
      Bound : Bound_T; Dim : Natural := 1) return Value_T
   is
      T : constant Entity_Id := Etype (Array_Node);
      R : Node_Id;
   begin
      if Is_Constrained (T) then
         R := Pick (List_Containing (First_Index (T)), Nat (Dim));
         return Emit_Expression
           (Env,
            (if Bound = Low then Low_Bound (R) else High_Bound (R)));
      else
         return Array_Bound
           (Env, Emit_LValue (Env, Array_Node), Bound, Dim);
      end if;
   end Array_Bound;

   ------------------
   -- Array_Length --
   ------------------

   function Array_Length (Env : Environ; Array_Node : Node_Id) return Value_T
   is
      Array_Type        : constant Entity_Id := Etype (Array_Node);
      First_Bound_Range : constant Entity_Id := First_Index (Array_Type);
      Result            : constant Value_T :=
        Bounds_To_Length
          (Env => Env,
           Low_Bound => Array_Bound (Env, Array_Node, Low),
           High_Bound => Array_Bound (Env, Array_Node, High),
           Bounds_Type => Etype (First_Bound_Range));

   begin
      Set_Value_Name (Result, "array-length");
      return Result;
   end Array_Length;

   ----------------
   -- Array_Size --
   ----------------

   function Array_Size
     (Env : Environ; Array_Type : Entity_Id;
      Containing_Record_Instance : Value_T := No_Value_T) return Value_T
   is
      CT       : constant Entity_Id := Component_Type (Array_Type);

      Size     : Value_T := No_Value_T;
      Cur_Size : Value_T;
      DSD      : Node_Id := First_Index (Array_Type);
      Dim      : Node_Id;
      T        : constant Type_T := Int_Ptr_Type;
      --  An array can be as big as the memory space, so use the appropriate
      --  type.

      function Emit_Bound (N : Node_Id) return Value_T;
      function Emit_Bound (N : Node_Id) return Value_T is

      begin
         if Size_Depends_On_Discriminant (Array_Type)
           and then Nkind (N) = N_Identifier
           --  The component is indeed a discriminant
           and then Nkind (Parent (Entity (N))) = N_Discriminant_Specification
         then
            return Env.Bld.Load
              (Env.Bld.Struct_GEP
                 (Containing_Record_Instance,
                  unsigned (UI_To_Int (Discriminant_Number (Entity (N))) - 1),
                  "field_access"), "field_load");
         else
            return Emit_Expression (Env, N);
         end if;
      end Emit_Bound;
   begin

      --  Go through every array dimension

      while Present (DSD) loop

         --  Compute the size of the dimension from the range bounds
         Dim := Get_Dim_Range (DSD);
         Cur_Size := Bounds_To_Length
           (Env         => Env,
            Low_Bound   => Emit_Bound (Low_Bound (Dim)),
            High_Bound  => Emit_Bound (High_Bound (Dim)),
            Bounds_Type => Etype (Low_Bound (Dim)));
         Cur_Size := Env.Bld.Z_Ext (Cur_Size, T, "array-size");

         --  Accumulate the product of the sizes
         --  If it's the first dimension, initialize our result with it
         --  Else, multiply our result by it

         if Size = No_Value_T then
            Size := Cur_Size;
         else
            Size := Env.Bld.Mul (Size, Cur_Size, "");
         end if;

         DSD := Next (DSD);
      end loop;

      --  If the component of the array is itself an array, then recursively
      --  compute the size of the component and return the product

      if Is_Array_Type (CT) then
         return Env.Bld.Mul (Size, Array_Size (Env, CT), "");
      else
         return Size;
      end if;

   end Array_Size;

end GNATLLVM.Arrays;
