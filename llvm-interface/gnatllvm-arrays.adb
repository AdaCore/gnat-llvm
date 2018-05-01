------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
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

with Sem_Eval; use Sem_Eval;
with Stand;    use Stand;
with Table;
with Uintp;    use Uintp;

with GNATLLVM.Compile;     use GNATLLVM.Compile;
with GNATLLVM.DebugInfo;   use GNATLLVM.DebugInfo;
with GNATLLVM.Records;     use GNATLLVM.Records;
with GNATLLVM.Utils;       use GNATLLVM.Utils;

---------------------
-- GNATLLVM.Arrays --
---------------------

package body GNATLLVM.Arrays is

   --  A bound of a constrained array can either be a compile-time
   --  constant, which we record as a Uint, the discriminant of an array,
   --  which we record as the Entity_Id of the E_Discriminant, or some
   --  dynamic value that was known at the declaration of the type.  We
   --  use the structures and table below to indicate which.  The value
   --  return by Get_Array_Info is the index into this table for the
   --  first index of a constrained array whose size isn't known at
   --  compile-time.  The remaining bounds are subsequent entries in the table.
   --
   --  For unconstrained arrays, we have an entry in the table for each
   --  dimension to record the type information abut each bound.

   type One_Bound is record
      Cnst    : Uint;
      Value   : Node_Id;
      Discr   : Entity_Id;
      Dynamic : Boolean;
   end record
     --  Only one item can be specified and the specification of Value or
     --  Discr means that Dynamic must be true.  We might think that exactly
     --  one item must be specified, but that's not the case for an
     --  unconstrained array.
     with Dynamic_Predicate => ((if Cnst = No_Uint then 0 else 1) +
                                (if No (Value) then 0 else 1) +
                                (if No (Discr) then 0 else 1)) <= 1
                                   and then ((No (Value) and then No (Discr))
                                             or else Dynamic);

   type Index_Bounds is record
      Bound_Type        : Entity_Id;
      Low, High         : One_Bound;
   end record
     with Dynamic_Predicate => Is_Discrete_Type (Bound_Type);

   package Array_Info is new Table.Table
     (Table_Component_Type => Index_Bounds,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 1024,
      Table_Increment      => 100,
      Table_Name           => "Array_Info_Table");
   --  Table of representation of arrays indexes

   function Type_For_Get_Bound
     (TE : Entity_Id; V : GL_Value) return Entity_Id
     with Pre  => Is_Array_Type (TE),
          Post => Is_Array_Type (Type_For_Get_Bound'Result);
   --  Get the best type to use to search for a bound of an arrray

   function Build_One_Bound
     (N : Node_Id; Unconstrained : Boolean) return One_Bound
     with Pre => Present (N);
   --  Helper function to build a One_Bound object from N

   function Create_String_Literal_Type
     (TE : Entity_Id; Comp_Typ : Type_T) return Type_T
     with Pre  => Ekind (TE) = E_String_Literal_Subtype
                  and then Present (Comp_Typ),
          Post => (Get_Type_Kind (Create_String_Literal_Type'Result) =
                     Array_Type_Kind);
   --  Helper function to create type for string literals

   function Bound_Complexity
     (B : One_Bound; For_Type : Boolean) return Integer is
      (if B.Cnst /= No_Uint then 0 elsif Present (B.Value) then 1
       elsif For_Type then 1 else 2);

   function Get_Dim_Range (N : Node_Id) return Node_Id
     with Pre  => Present (N), Post => Present (Get_Dim_Range'Result);
   --  Return the N_Range for an array type

   function Get_GEP_Safe_Type (V : GL_Value) return Entity_Id
     with Pre  => not Is_Reference (V),
          Post => Is_Integer_Type (Get_GEP_Safe_Type'Result);
   --  GEP treats array indices as signed values.  If the type is unsigned
   --  (including Boolean; see C55C02B), it will sign-extend rather than
   --  zero-extend the value.  So if this type is smaller than the size of
   --  a pointer and is unsigned, we must return a wider type.

   ---------------------
   -- Build_One_Bound --
   ---------------------

   function Build_One_Bound
     (N : Node_Id; Unconstrained : Boolean) return One_Bound is
   begin
      if Unconstrained then
         return (Cnst => No_Uint, Value => Empty, Discr => Empty,
                 Dynamic => True);
      elsif Compile_Time_Known_Value (N) then
         return (Cnst => Expr_Value (N), Value => Empty, Discr => Empty,
                 Dynamic => not UI_Is_In_Int_Range (Expr_Value (N)));
      elsif Is_Entity_Name (N)
        and then Ekind (Entity (N)) = E_Discriminant
      then
         return (Cnst => No_Uint, Value => Empty,
                 Discr => Original_Record_Component (Entity (N)),
                 Dynamic => True);
      else
         return (Cnst => No_Uint, Discr => Empty,
                 Value => N, Dynamic => True);
      end if;

   end Build_One_Bound;

   ------------------------
   -- Type_For_Get_Bound --
   ------------------------

   function Type_For_Get_Bound
     (TE : Entity_Id; V : GL_Value) return Entity_Id
   is
      V_Type : constant Entity_Id :=
        (if No (V) then Empty elsif Is_Access_Type (V)
         then Full_Designated_Type (V) else Full_Etype (V));

   begin
      --  If only TE is around, use it.  Likewise if V_Type is not an
      --  array type or not related to TE.  Otherwise, use the type
      --  that's constrained, preferring V's type.

      if No (V_Type) or else not Is_Array_Type (V_Type)
        or else (Implementation_Base_Type (V_Type) /=
                   Implementation_Base_Type (TE))
      then
         return TE;
      elsif not Is_Constrained (V_Type) and then Is_Constrained (TE) then
         return TE;
      else
         return V_Type;
      end if;
   end Type_For_Get_Bound;

   ---------------------
   -- Get_Array_Bound --
   ---------------------

   function Get_Array_Bound
     (TE       : Entity_Id;
      Dim      : Nat;
      Is_Low   : Boolean;
      V        : GL_Value;
      For_Type : Boolean := False) return GL_Value
   is
      Typ        : constant Entity_Id    := Type_For_Get_Bound (TE, V);
      Info_Idx   : constant Nat          := Get_Array_Info (Typ);
      Dim_Info   : constant Index_Bounds := Array_Info.Table (Info_Idx + Dim);
      Bound_Info : constant One_Bound    :=
        (if Is_Low then Dim_Info.Low else Dim_Info.High);
      Bound_Idx    : constant Nat := Dim  * 2 + (if Is_Low then 0 else 1);
      --  In the array fat pointer bounds structure, bounds are stored as a
      --  sequence of (lower bound, upper bound) pairs.
      Result       : GL_Value;

   begin
      Push_Debug_Freeze_Pos;

      --  There are four cases: a constant size, in which case we return
      --  that size, a saved value, in which case we return that value,
      --  an unconstrained array, in which case we have a fat pointer and
      --  extract the bounds from it, or a discriminant, in which case we
      --  access that field of the enclosing record.

      if Bound_Info.Cnst /= No_Uint then
         Result := Const_Int (Dim_Info.Bound_Type, Bound_Info.Cnst);
      elsif Present (Bound_Info.Value) then
         Result := Build_Type_Conversion
           (Bound_Info.Value, Dim_Info.Bound_Type);
      elsif not Is_Constrained (TE) then
         Result := Extract_Value
           (Dim_Info.Bound_Type, V, (1 => 1, 2 => Integer (Bound_Idx)),
            (if Is_Low then "low-bound" else "high-bound"));

      else
         --  We now should have the discriminated case.  Make sure we do.

         pragma Assert (Ekind (Bound_Info.Discr) = E_Discriminant);

         --  If we are getting the size of a type, as opposed to a value,
         --  we have to use the first/last value of the range of the type
         --  of the discriminant.

         if For_Type then
            declare
               Disc_Type : constant Entity_Id := Full_Etype (Bound_Info.Discr);
            begin
               Result := Build_Type_Conversion
                 ((if Is_Low then Type_Low_Bound (Disc_Type)
                   else Type_High_Bound (Disc_Type)), Dim_Info.Bound_Type);
            end;
         else
            Result := Convert_To_Elementary_Type
              (Load (Record_Field_Offset
                       (Get_Matching_Value (Full_Scope (Bound_Info.Discr)),
                        Bound_Info.Discr)),
               Dim_Info.Bound_Type);
         end if;
      end if;

      Pop_Debug_Freeze_Pos;
      return Result;
   end Get_Array_Bound;

   ----------------------
   -- Get_Array_Length --
   ----------------------

   function Get_Array_Length
     (TE       : Entity_Id;
      Dim      : Nat;
      V        : GL_Value;
      For_Type : Boolean := False) return GL_Value
   is
      Low_Bound  : constant GL_Value :=
        Get_Array_Bound (TE, Dim, True, V, For_Type);
      High_Bound : constant GL_Value :=
        Get_Array_Bound (TE, Dim, False, V, For_Type);

   begin
      --  The length of an array that has the maximum range of its type is
      --  not representable in that type (it's one too high).  Rather than
      --  trying to find some suitable type, we use Size_Type, which will
      --  also make thing simpler for some of our callers.

      return Bounds_To_Length (Low_Bound, High_Bound, Size_Type);
   end Get_Array_Length;

   -------------------------------
   -- Get_Array_Size_Complexity --
   -------------------------------

   function Get_Array_Size_Complexity
     (TE : Entity_Id; For_Type : Boolean := False) return Natural
   is
      Complexity  : Natural :=
        Get_Type_Size_Complexity (Full_Component_Type (TE), True);
      Info_Idx    : constant Nat := Get_Array_Info (TE);

   begin
      for Dim in 0 .. Number_Dimensions (TE) - 1 loop
         declare
            Dim_Info : constant Index_Bounds
              := Array_Info.Table (Info_Idx + Dim);
         begin
            Complexity := (Complexity +
                             Bound_Complexity (Dim_Info.Low, For_Type) +
                             Bound_Complexity (Dim_Info.High, For_Type));
         end;
      end loop;

      return Complexity;
   end Get_Array_Size_Complexity;

   --------------------------------
   -- Create_String_Literal_Type --
   --------------------------------

   function Create_String_Literal_Type
     (TE : Entity_Id; Comp_Typ : Type_T) return Type_T
   is
      First      : constant Uint         :=
        Get_Uint_Value (String_Literal_Low_Bound (TE));
      Length     : constant Uint         := String_Literal_Length (TE);
      Last       : constant Uint         := First + Length - 1;
      Low_Bound  : constant One_Bound    := (Cnst => First, Value => Empty,
                                         Discr => Empty, Dynamic => False);
      High_Bound : constant One_Bound    := (Cnst => Last, Value => Empty,
                                          Discr => Empty, Dynamic => False);
      Dim_Info   : constant Index_Bounds := (Bound_Type => Standard_Positive,
                                             Low => Low_Bound,
                                             High => High_Bound);
      Result_Typ : constant Type_T       :=
        Array_Type (Comp_Typ, unsigned (UI_To_Int (Length)));

   begin
      --  It's redundant to set the type here, since our caller will set it,
      --  but we have to set it in order to set the array info.

      Set_Type (TE, Result_Typ);
      Array_Info.Append (Dim_Info);
      Set_Array_Info (TE, Array_Info.Last);
      return Result_Typ;

   end Create_String_Literal_Type;

   -----------------------
   -- Create_Array_Type --
   -----------------------

   function Create_Array_Type (TE : Entity_Id) return Type_T is
      Unconstrained     : constant Boolean   := not Is_Constrained (TE);
      Comp_Type         : constant Entity_Id := Full_Component_Type (TE);
      Must_Use_Opaque   : Boolean            := Is_Dynamic_Size (Comp_Type);
      This_Dynamic_Size : Boolean            :=
        Must_Use_Opaque or Unconstrained;
      Typ               : Type_T             := Create_Type (Comp_Type);
      --  This must be before the next line because it may recurse
      First_Info        : constant Nat       := Array_Info.Last + 1;
      Dim               : Nat                := 0;
      Index             : Entity_Id;

   begin
      if Ekind (TE) = E_String_Literal_Subtype then
         return Create_String_Literal_Type (TE, Typ);
      end if;

      --  We loop through each dimension of the array creating the entries
      --  for Array_Info.  If the component type is of variable size or if
      --  either bound of an index is a dynamic size, this type is of
      --  dynamic size.  We must use an opaque type if this is of dynamic size
      --  unless the only reason it's dynamic is because the first dimension
      --  is of variable-size: in that case, we can use an LLVM array with
      --  zero as the bound.

      --  ?? There is one more case that we could try, which is in the
      --  multi-dimensional case of dynamic bounds, but when the component
      --  type is of fixed size.  In that case, we could flatten this to a
      --  single-dimension array of zero size, and do the indexing
      --  computation using the component size rather than by byte. However,
      --  there doesn't appear to be a simple way of realizing that we've
      --  done this, so don't try it just now.

      Index := First_Index (TE);
      while Present (Index) loop
         declare
            Idx_Range : constant Node_Id        := Get_Dim_Range (Index);
            --  Sometimes, the frontend leaves an identifier that
            --  references an integer subtype instead of a range.

            Index_Type : constant Entity_Id     := Full_Etype (Index);
            Index_Base : constant Entity_Id     :=
              Implementation_Base_Type (Index_Type);

            LB          : constant Node_Id      := Low_Bound (Idx_Range);
            HB          : constant Node_Id      := High_Bound (Idx_Range);
            Dim_Info    : constant Index_Bounds :=
              (Bound_Type => Index_Base,
               Low => Build_One_Bound (LB, Unconstrained),
               High => Build_One_Bound (HB, Unconstrained));
            --  We have to be careful here and flag the type of the index
            --  from that of the base type since we can have index ranges
            --  that are outside the base type if the subtype is superflat
            --  (see C37172C).

         begin
            --  Update whether or not this will be of dynamic size and
            --  whether we must use an opaque type based on this dimension.
            --  Then record it.  Note that LLVM only allows the range of an
            --  array to be in the range of "unsigned".  So we have to treat
            --  a too-large constant as if it's of variable size.

            if Dim_Info.Low.Dynamic or else Dim_Info.High.Dynamic then
               This_Dynamic_Size := True;
               if Dim /= 0 then
                  Must_Use_Opaque := True;
               end if;
            end if;

            Array_Info.Append (Dim_Info);
            Index := Next_Index (Index);
            Dim := Dim + 1;
         end;
      end loop;

      --  If we must use an opaque type, make one.  Otherwise loop through
      --  the types making the LLVM type.

      if Must_Use_Opaque then
         Typ := Struct_Create_Named (LLVM_Context, "");
      else
         for I in reverse First_Info .. Array_Info.Last loop
            declare
               Dim_Info : constant Index_Bounds := Array_Info.Table (I);
               Low      : constant One_Bound    := Dim_Info.Low;
               High     : constant One_Bound    := Dim_Info.High;
               Dynamic  : constant Boolean      := Low.Dynamic or High.Dynamic;
               Rng      : Long_Long_Integer     := 0;
            begin
               if not Dynamic and then Low.Cnst <= High.Cnst then
                  Rng := UI_To_Long_Long_Integer (High.Cnst - Low.Cnst + 1);
               end if;

               Typ := Array_Type (Typ, unsigned (Rng));
            end;
         end loop;
      end if;

      --  It's redundant to set the type here, since our caller will set it,
      --  but we have to set it in order to set the array info.

      Set_Type (TE, Typ);
      Set_Dynamic_Size (TE, This_Dynamic_Size);
      Set_Array_Info (TE, First_Info);

      return Typ;
   end Create_Array_Type;

   ------------------------------
   -- Create_Array_Bounds_Type --
   ------------------------------

   function Create_Array_Bounds_Type (TE : Entity_Id) return Type_T
   is
      Dims       : constant Nat := Number_Dimensions (TE);
      Fields     : aliased Type_Array (Nat range 0 .. 2 * Dims - 1);
      First_Info : constant Nat := Get_Array_Info (TE);
      J          : Nat          := 0;

   begin
      for I in Nat range 0 .. Dims - 1 loop
         Fields (J) :=
           Create_Type (Array_Info.Table (First_Info + I).Bound_Type);
         Fields (J + 1) := Fields (J);
         J := J + 2;
      end loop;

      return Build_Struct_Type (Fields);
   end Create_Array_Bounds_Type;

   -----------------------------------
   -- Create_Array_Raw_Pointer_Type --
   -----------------------------------

   function Create_Array_Raw_Pointer_Type (TE : Entity_Id) return Type_T is
      Elt_Type : constant Type_T := Create_Type (Full_Component_Type (TE));
      Arr_Type : constant Type_T := Array_Type (Elt_Type, 0);

   begin
      return Pointer_Type (Arr_Type, 0);
   end Create_Array_Raw_Pointer_Type;

   -----------------------------------
   -- Create_Array_Fat_Pointer_Type --
   -----------------------------------

   function Create_Array_Fat_Pointer_Type (TE : Entity_Id) return Type_T is
   begin
      return Build_Struct_Type
        ((1 => Create_Array_Raw_Pointer_Type (TE),
          2 => Create_Array_Bounds_Type (TE)));
   end Create_Array_Fat_Pointer_Type;

   ------------------------
   -- Get_Array_Elements --
   ------------------------

   function Get_Array_Elements
     (V        : GL_Value;
      TE       : Entity_Id;
      For_Type : Boolean := False) return GL_Value
   is
      Size : GL_Value := Size_Const_Int (Uint_1);

   begin
      --  Go through every array dimension.  Get its size and multiply all
      --  of them together.

      for Dim in Nat range 0 .. Number_Dimensions (TE) - 1 loop
         Size := NSW_Mul (Size, Get_Array_Length (TE, Dim, V, For_Type));
      end loop;

      return Size;
   end Get_Array_Elements;

   -------------------------
   -- Get_Array_Type_Size --
   -------------------------

   function Get_Array_Type_Size
     (TE       : Entity_Id;
      V        : GL_Value;
      For_Type : Boolean := False) return GL_Value
   is
      Comp_Type     : constant Entity_Id := Full_Component_Type (TE);
      Comp_Size     : constant GL_Value  :=
        Get_Type_Size (Comp_Type, For_Type => True);
      Num_Elements  : constant GL_Value  :=
        Get_Array_Elements (V, TE, For_Type);

   begin
      return NSW_Mul
        (Convert_To_Size_Type (Comp_Size), Convert_To_Size_Type (Num_Elements),
         "size");
   end Get_Array_Type_Size;

   --------------------------
   -- Emit_Array_Aggregate --
   --------------------------

   function Emit_Array_Aggregate
     (N              : Node_Id;
      Dims_Left      : Pos;
      Indices_So_Far : Index_Array;
      Value_So_Far   : GL_Value) return GL_Value
   is
      Comp_Type : constant Entity_Id := Full_Component_Type (Full_Etype (N));
      Cur_Index : Integer            := 0;
      Cur_Value : GL_Value           := Value_So_Far;
      Expr      : Node_Id;

   begin

      pragma Assert (not Is_Dynamic_Size (Comp_Type));
      --  The code below, by using Insert_Value, restricts itself to
      --  Components of fixed sizes.  But that's OK because the front end
      --  handles those cases.

      Expr := First (Expressions (N));
      while Present (Expr) loop

         --  If this is a nested N_Aggregate and we have dimensions left
         --  in the outer array, use recursion to fill in the aggregate.

         if Nkind_In (Expr, N_Aggregate, N_Extension_Aggregate)
           and then Dims_Left > 1
         then
            Cur_Value := Emit_Array_Aggregate
              (Expr, Dims_Left - 1, Indices_So_Far & (1 => Cur_Index),
               Cur_Value);

         else
            Cur_Value := Insert_Value
              (Cur_Value, Build_Type_Conversion (Expr, Comp_Type),
               Indices_So_Far & (1 => Cur_Index));
         end if;

         Cur_Index := Cur_Index + 1;
         Next (Expr);
      end loop;

      return Cur_Value;
   end Emit_Array_Aggregate;

   ----------------
   -- Array_Data --
   ----------------

   function Array_Data (V : GL_Value) return GL_Value is
      Arr_Type : constant Entity_Id := Full_Designated_Type (V);
   begin
      if Is_Raw_Array (V) or else Is_Constrained (Arr_Type)
      then
         return V;
      else
         return Extract_Value_To_Raw_Array (Arr_Type, V, 0, "array-data");
      end if;
   end Array_Data;

   -----------------------
   -- Array_Fat_Pointer --
   -----------------------

   function Array_Fat_Pointer (TE : Entity_Id; V : GL_Value) return GL_Value is
      Src_Type       : constant Entity_Id := Full_Designated_Type (V);
      Info_Idx       : constant Nat       := Get_Array_Info (TE);
      Fat_Ptr        : GL_Value           := Get_Undef_Ref (TE);
      Array_Data_Ptr : constant GL_Value  := Ptr_To_Raw_Array (V, TE);

   begin

      for Dim in Nat range 0 .. Number_Dimensions (TE) - 1 loop

         declare
            --  The type of the bound of the array we're using for the bounds
            --  may not be the same as the type of the bound in the
            --  unconstrained array, so be sure to convert (C46042A).

            Bound_Type     : constant Entity_Id      :=
              Array_Info.Table (Info_Idx + Dim).Bound_Type;
            Low_Bound    : constant GL_Value         :=
              Get_Array_Bound (Src_Type, Dim, True, V);
            High_Bound   : constant GL_Value         :=
              Get_Array_Bound (Src_Type, Dim, False, V);
            Converted_Low_Bound : constant GL_Value  :=
              Convert_To_Elementary_Type (Low_Bound, Bound_Type);
            Converted_High_Bound : constant GL_Value :=
              Convert_To_Elementary_Type (High_Bound, Bound_Type);

         begin
            Fat_Ptr := Insert_Value
              (Fat_Ptr, Converted_Low_Bound, (1 => 1, 2 => Integer (Dim * 2)));

            Fat_Ptr := Insert_Value
              (Fat_Ptr, Converted_High_Bound,
               (1 => 1, 2 => Integer (Dim * 2 + 1)));
         end;
      end loop;

      return Insert_Value (Fat_Ptr, Array_Data_Ptr, 0);
   end Array_Fat_Pointer;

   ------------------------
   -- Update_Fat_Pointer --
   ------------------------

   function Update_Fat_Pointer
     (Fat_Ptr : GL_Value; Array_Data : GL_Value) return GL_Value is
   begin
      return Insert_Value (Fat_Ptr, Array_Data, 0);
   end Update_Fat_Pointer;

   -----------------------
   -- Get_GEP_Safe_Type --
   -----------------------

   function Get_GEP_Safe_Type (V : GL_Value) return Entity_Id is
      Int_Types : constant array (Integer range <>) of Entity_Id :=
        (Standard_Short_Short_Integer, Standard_Short_Integer,
         Standard_Integer, Standard_Long_Integer, Standard_Long_Long_Integer);
      Our_Type  : constant Entity_Id := Full_Etype (V);

   begin
      --  If we are of an unsigned type narrower than Size_Type, we must find
      --  a wider type to use.  We use the first, which will be the narrowest.

      if not Is_Unsigned_Type (Our_Type)
        or else RM_Size (Our_Type) >= RM_Size (Size_Type)
      then
         return Our_Type;
      end if;

      for Typ of Int_Types loop
         if RM_Size (Typ) > RM_Size (Our_Type) then
            return Typ;
         end if;
      end loop;

      return Empty;
   end Get_GEP_Safe_Type;

   ------------------------
   -- Get_Indexed_LValue --
   ------------------------

   function Get_Indexed_LValue
     (Indexes : List_Id; V : GL_Value) return GL_Value
   is
      Array_Type     : constant Entity_Id := Full_Designated_Type (V);
      Comp_Type      : constant Entity_Id := Full_Component_Type (Array_Type);
      Array_Data_Ptr : constant GL_Value  := Array_Data (V);
      Idxs : GL_Value_Array (1 .. List_Length (Indexes) + 1) :=
        (1 => Size_Const_Null, others => <>);
      --  Operands for the GetElementPtr instruction: one for the
      --  pointer deference, and then one per array index.

      J : Nat := 2;
      N : Node_Id;

   begin
      N := First (Indexes);
      while Present (N) loop

         --  Adjust the index according to the range lower bound

         declare
            User_Index    : constant GL_Value       := Emit_Expression (N);
            Dim_Low_Bound : constant GL_Value       :=
              Get_Array_Bound (Array_Type, J - 2, True, V);
            Dim_Op_Type   : constant Entity_Id      :=
              Get_GEP_Safe_Type (Dim_Low_Bound);
            Converted_Index : constant GL_Value     :=
              Convert_To_Elementary_Type (User_Index, Dim_Op_Type);
            Converted_Low_Bound : constant GL_Value :=
              Convert_To_Elementary_Type (Dim_Low_Bound, Dim_Op_Type);

         begin
            Idxs (J) := NSW_Sub
              (Converted_Index, Converted_Low_Bound, "index");
         end;

         J := J + 1;
         N := Next (N);
      end loop;

      --  There are two approaches we can take here.  If we haven't used
      --  an opaque type, we can just do a GEP with the values above.

      if Type_Is_Sized (Create_Type (Array_Type)) then
         return GEP (Comp_Type, Array_Data_Ptr, Idxs, "array-element-access");
      end if;

      --  Otherwise, we convert the array data type to an i8*, compute the
      --  byte offset from the index and size information, index that, and
      --  then convert back to the array type.  We start with the first
      --  index then for each dimension after the first, multiply by the
      --  size of that dimension and add that index.  Finally, we multiply
      --  by the size of the component.  We do all of this in Size_Type.

      declare
         Data          : constant GL_Value :=
           Pointer_Cast (Array_Data_Ptr, Standard_A_Char);
         Comp_Size     : constant GL_Value :=
           Get_Type_Size (Comp_Type, For_Type => True);
         Index         : GL_Value          := Convert_To_Size_Type (Idxs (2));

      begin

         for Dim in 1 .. Number_Dimensions (Array_Type) - 1 loop
            Index := NSW_Add (NSW_Mul (Index,
                                       Get_Array_Length (Array_Type, Dim, V)),
                              Convert_To_Size_Type (Idxs (Dim + 2)));
         end loop;

         Index := NSW_Mul (Index, Comp_Size);
         return Ptr_To_Ref
           (GEP (Standard_Short_Short_Integer, Data, (1 => Index),
                 "gen-index"),
            Comp_Type);
      end;

   end Get_Indexed_LValue;

   ----------------------
   -- Get_Slice_LValue --
   ----------------------

   function Get_Slice_LValue
     (TE  : Entity_Id;
      Rng : Node_Id;
      V   : GL_Value) return GL_Value
   is
      Array_Data_Ptr : constant GL_Value      := Array_Data (V);
      Arr_Type       : constant Entity_Id     := Full_Designated_Type (V);
      Low_Idx_Bound  : constant GL_Value      :=
        Get_Array_Bound (Arr_Type, 0, True, V);
      Index_Val      : constant GL_Value      :=
        Emit_Expression (Low_Bound (Rng));
      Dim_Op_Type   : constant Entity_Id      :=
        Get_GEP_Safe_Type (Low_Idx_Bound);
      Converted_Index : constant GL_Value     :=
        Convert_To_Elementary_Type (Index_Val, Dim_Op_Type);
      Converted_Low_Bound : constant GL_Value :=
        Convert_To_Elementary_Type (Low_Idx_Bound, Dim_Op_Type);

      --  Compute how much we need to offset the array pointer. Slices
      --  can be built only on single-dimension arrays

      Index_Shift : constant GL_Value :=
        NSW_Sub (Converted_Index, Converted_Low_Bound, "offset");

   begin
      --  Like the above case, we have to hande both the opaque and non-opaque
      --  cases.  Luckily, we know we're only a single dimension.  However,
      --  GEP's result type is a pointer to the component type, so we need
      --  to cast to the result (array) type in both cases.

      if Type_Is_Sized (Create_Type (Arr_Type)) then
         return Ptr_To_Ref (GEP (TE, Array_Data_Ptr,
                                 (Size_Const_Null, Index_Shift),
                                 "array-shifted"), TE);
      end if;

      declare
         Data          : constant GL_Value  :=
           Pointer_Cast (Array_Data_Ptr, Standard_A_Char);
         Comp_Type     : constant Entity_Id := Full_Component_Type (Arr_Type);
         Comp_Size     : constant GL_Value  :=
           Get_Type_Size (Comp_Type, For_Type => True);
         Index         : constant GL_Value  :=
           NSW_Mul (Comp_Size, Convert_To_Size_Type (Index_Shift));
      begin
         return Ptr_To_Ref
           (GEP (Arr_Type, Data, (1 => Index), "gen-index"), TE);
      end;

   end Get_Slice_LValue;

   -------------------
   -- Get_Dim_Range --
   -------------------

   function Get_Dim_Range (N : Node_Id) return Node_Id is
   begin
      case Nkind (N) is
         when N_Range =>
            return N;
         when N_Identifier =>
            return Scalar_Range (Entity (N));

         when N_Subtype_Indication =>
            declare
               Constr : constant Node_Id := Constraint (N);
            begin
               if Present (Constr) then
                  if Nkind (Constr) = N_Range_Constraint then
                     return Range_Expression (Constr);
                  end if;
               else
                  return Scalar_Range (Entity (Subtype_Mark (N)));
               end if;
            end;

         when others =>
            null;
      end case;

      raise Program_Error
        with "Invalid node kind in context: " & Node_Kind'Image (Nkind (N));
      pragma Annotate (Xcov, Exempt_Off);
   end Get_Dim_Range;

end GNATLLVM.Arrays;
