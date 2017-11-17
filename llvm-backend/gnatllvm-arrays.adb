------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2017, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

with Atree;  use Atree;
with Nlists; use Nlists;
with Sinfo;  use Sinfo;
with Stand;  use Stand;
with Uintp;  use Uintp;

with LLVM.Core; use LLVM.Core;
with GNATLLVM.Compile; use GNATLLVM.Compile;
with GNATLLVM.Types;   use GNATLLVM.Types;

---------------------
-- GNATLLVM.Arrays --
---------------------

package body GNATLLVM.Arrays is

   function Get_Bound_Index (Dim : Natural; Bound : Bound_T) return unsigned;
   --  An array fat pointer embbeds a structure holding the bounds of the
   --  array. This returns the index for some bound given its dimension
   --  inside the array and on whether this is the lower or the upper bound.

   ---------------------
   -- Get_Bound_Index --
   ---------------------

   function Get_Bound_Index (Dim : Natural; Bound : Bound_T) return unsigned is
      Bounds_Pair_Idx : constant Natural := (Dim - 1) * 2;
      --  In the array fat pointer bounds structure, bounds are stored as a
      --  sequence of (lower bound, upper bound) pairs: get the offset of
      --  such a pair.
   begin
      return unsigned (Bounds_Pair_Idx + (if Bound = Low then 0 else 1));
   end Get_Bound_Index;

   ------------------------
   -- Extract_Array_Info --
   ------------------------

   procedure Extract_Array_Info
     (Env         : Environ;
      Array_Node  : Node_Id;
      Array_Descr : out Value_T;
      Array_Type  : out Entity_Id) is
   begin
      Array_Type := Etype (Array_Node);
      Array_Descr :=
        (if Is_Constrained (Array_Type)
         then No_Value_T
         else Emit_LValue (Env, Array_Node));
   end Extract_Array_Info;

   -----------------
   -- Array_Bound --
   -----------------

   function Array_Bound
     (Env         : Environ;
      Array_Descr : Value_T;
      Array_Type  : Entity_Id;
      Bound       : Bound_T;
      Dim         : Natural := 1) return Value_T
   is
   begin
      if Is_Constrained (Array_Type) then
         declare
            Indices_List  : constant List_Id :=
              List_Containing (First_Index (Array_Type));
            Index_Subtype : constant Node_Id :=
              Etype (Pick (Indices_List, Nat (Dim)));
         begin
            return Emit_Expression
              (Env,
               (if Bound = Low
                then Type_Low_Bound (Index_Subtype)
                else Type_High_Bound (Index_Subtype)));
         end;

      else
         --  Array_Descr must be a fat pointer

         declare
            Array_Bounds : constant Value_T :=
              Extract_Value (Env.Bld, Array_Descr, 1, "array-bounds");
            --  Get the structure that contains array bounds
         begin
            return Extract_Value
              (Env.Bld,
               Array_Bounds,
               Get_Bound_Index (Dim, Bound),
               (if Bound = Low
                then "low-bound"
                else "high-bound"));
         end;
      end if;
   end Array_Bound;

   ------------------
   -- Array_Length --
   ------------------

   function Array_Length
     (Env         : Environ;
      Array_Descr : Value_T;
      Array_Type  : Entity_Id) return Value_T
   is
      First_Bound_Range : constant Entity_Id := First_Index (Array_Type);
      Result            : constant Value_T :=
        Bounds_To_Length
          (Env => Env,
           Low_Bound => Array_Bound (Env, Array_Descr, Array_Type, Low),
           High_Bound => Array_Bound (Env, Array_Descr, Array_Type, High),
           Bounds_Type => Etype (First_Bound_Range));

   begin
      Set_Value_Name (Result, "array-length");
      return Result;
   end Array_Length;

   ----------------
   -- Array_Size --
   ----------------

   function Array_Size
     (Env                        : Environ;
      Array_Descr                : Value_T;
      Array_Type                 : Entity_Id;
      Containing_Record_Instance : Value_T := No_Value_T) return Value_T
   is
      function Emit_Bound (N : Node_Id) return Value_T;
      --  Emit code to compute N as an array bound of a constrained arary,
      --  handling bounds that come from record discriminants.

      ----------------
      -- Emit_Bound --
      ----------------

      function Emit_Bound (N : Node_Id) return Value_T is
      begin
         if Size_Depends_On_Discriminant (Array_Type)
           and then Nkind (N) = N_Identifier
         --  The component is indeed a discriminant
           and then Nkind (Parent (Entity (N))) = N_Discriminant_Specification
         then
            return Load
              (Env.Bld,
               Struct_GEP
                 (Env.Bld,
                  Containing_Record_Instance,
                  unsigned (UI_To_Int (Discriminant_Number (Entity (N))) - 1),
                  "field_access"), "field_load");
         else
            return Emit_Expression (Env, N);
         end if;
      end Emit_Bound;

      Constrained : constant Boolean := Is_Constrained (Array_Type);

      Size        : Value_T := No_Value_T;
      Size_Type   : constant Type_T := Int_Ptr_Type;
      --  Type for the result. An array can be as big as the memory space, so
      --  use a type as large as pointers.

      DSD         : Node_Id := First_Index (Array_Type);
      Dim         : Node_Id;
      Dim_Index   : Natural;
      Dim_Length  : Value_T;

      --  Start of processing for Array_Size

   begin
      Size := Const_Int (Size_Type, 1, Sign_Extend => LLVM.Types.False);

      --  Go through every array dimension

      Dim_Index := 1;
      while Present (DSD) loop

         --  Compute the length of the dimension from the range bounds

         Dim := Get_Dim_Range (DSD);
         Dim_Length := Bounds_To_Length
           (Env         => Env,
            Low_Bound   =>
              (if Constrained
               then Emit_Bound (Low_Bound (Dim))
               else Array_Bound
                 (Env, Array_Descr, Array_Type, Low, Dim_Index)),
            High_Bound  =>
              (if Constrained
               then Emit_Bound (High_Bound (Dim))
               else Array_Bound
                 (Env, Array_Descr, Array_Type, High, Dim_Index)),
            Bounds_Type => Etype (Low_Bound (Dim)));
         Dim_Length :=
           Z_Ext (Env.Bld, Dim_Length, Size_Type, "array-dim-length");

         --  Accumulate the product of the sizes

         Size := Mul (Env.Bld, Size, Dim_Length, "");

         DSD := Next (DSD);
         Dim_Index := Dim_Index + 1;
      end loop;

      return Size;
   end Array_Size;

   ----------------
   -- Array_Data --
   ----------------

   function Array_Data
     (Env         : Environ;
      Array_Descr : Value_T;
      Array_Type  : Entity_Id) return Value_T is
   begin
      if Is_Constrained (Array_Type) then
         return Array_Descr;
      else
         return Extract_Value (Env.Bld, Array_Descr, 0, "array-data");
      end if;
   end Array_Data;

   -----------------------
   -- Array_Fat_Pointer --
   -----------------------

   function Array_Fat_Pointer
     (Env        : Environ;
      Array_Data : Value_T;
      Array_Type : Entity_Id) return Value_T
   is
      Fat_Ptr_Type      : constant Type_T :=
        Create_Array_Fat_Pointer_Type (Env, Array_Type);
      Fat_Ptr_Elt_Types : aliased Type_Array (1 .. 2);

      Array_Data_Type   : Type_T renames Fat_Ptr_Elt_Types (1);
      Array_Bounds_Type : Type_T renames Fat_Ptr_Elt_Types (2);

      Fat_Ptr        : Value_T := Get_Undef (Fat_Ptr_Type);
      Array_Data_Ptr : Value_T;
      Bounds         : Value_T;
      Dim_I          : Integer;
      R              : Node_Id;

   begin
      pragma Assert (Count_Struct_Element_Types (Fat_Ptr_Type) = 2);
      Get_Struct_Element_Types (Fat_Ptr_Type, Fat_Ptr_Elt_Types'Address);

      Array_Data_Ptr :=
        Pointer_Cast (Env.Bld, Array_Data, Array_Data_Type, "");
      Bounds := Get_Undef (Array_Bounds_Type);

      --  Fill Bounds with actual array bounds

      if Ekind (Array_Type) = E_String_Literal_Subtype then
         declare
            Low : constant Uint :=
              Intval (String_Literal_Low_Bound (Array_Type));
            Typ : constant Type_T := Create_Type (Env, Standard_Positive);

         begin
            Bounds := Insert_Value
              (Env.Bld,
               Bounds,
               Const_Int
                 (Typ,
                  unsigned_long_long (UI_To_Int (Low)),
                  Sign_Extend => LLVM.Types.False),
               0,
               "");
            Bounds := Insert_Value
              (Env.Bld,
               Bounds,
               Const_Int
                 (Typ,
                  unsigned_long_long
                    (UI_To_Int (String_Literal_Length (Array_Type) - Low + 1)),
                  Sign_Extend => LLVM.Types.False),
               1,
               "");
         end;
      else
         Dim_I := 1;

         for Dim of Iterate (List_Containing (First_Index (Array_Type))) loop
            R := Get_Dim_Range (Dim);
            Bounds := Insert_Value
              (Env.Bld,
               Bounds,
               Emit_Expression (Env, Low_Bound (R)),
               Get_Bound_Index (Dim_I, Low),
               "");
            Bounds := Insert_Value
              (Env.Bld,
               Bounds,
               Emit_Expression (Env, High_Bound (R)),
               Get_Bound_Index (Dim_I, High),
               "");
            Dim_I := Dim_I + 1;
         end loop;
      end if;

      --  Then fill the fat pointer itself
      Fat_Ptr := Insert_Value (Env.Bld, Fat_Ptr, Array_Data_Ptr, 0, "");
      Fat_Ptr := Insert_Value (Env.Bld, Fat_Ptr, Bounds, 1, "");

      return Fat_Ptr;
   end Array_Fat_Pointer;

end GNATLLVM.Arrays;
