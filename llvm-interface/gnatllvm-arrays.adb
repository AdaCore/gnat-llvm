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

with Atree;      use Atree;
with Sem_Eval;   use Sem_Eval;
with Sinfo;      use Sinfo;
with Stand;      use Stand;
with Table;
with Uintp;      use Uintp;

with GNATLLVM.Compile; use GNATLLVM.Compile;
with GNATLLVM.Types;   use GNATLLVM.Types;

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
   --  Table of representation of arrays indexes.

   function Get_And_Create_Array_Info
     (Env : Environ; TE : Entity_Id) return Nat
     with Pre  => Env /= null and then Is_Array_Type (TE),
          Post => Get_And_Create_Array_Info'Result > 0
                  and then Has_Type (Env, TE);
   --  Utility function to verify that type is created and then get
   --  array info.

   function Build_One_Bound
     (N : Node_Id; Unconstrained : Boolean) return One_Bound
     with Pre => Present (N);
   --  Helper function to build a One_Bound object from N

   function Create_String_Literal_Type
     (Env : Environ; TE : Entity_Id; Comp_Typ : Type_T) return Type_T
     with Pre  => Env /= null and then Ekind (TE) = E_String_Literal_Subtype
                  and then Present (Comp_Typ),
          Post => (Get_Type_Kind (Create_String_Literal_Type'Result) =
                     Array_Type_Kind);
   --  Helper function to create type for string literals

   function Get_Dim_Range (N : Node_Id) return Node_Id
     with Pre  => Present (N), Post => Present (Get_Dim_Range'Result);
   --  Return the N_Range for an array type

   -------------------------------
   -- Get_And_Create_Array_Info --
   -------------------------------

   function Get_And_Create_Array_Info
     (Env : Environ; TE : Entity_Id) return Nat
   is
   begin
      Discard (Create_Type (Env, TE));
      return Get_Array_Info (Env, TE);
   end Get_And_Create_Array_Info;

   ---------------------
   -- Build_One_Bound --
   ---------------------

   function Build_One_Bound
     (N : Node_Id; Unconstrained : Boolean) return One_Bound
   is
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

   ---------------------
   -- Get_Array_Bound --
   ---------------------

   function Get_Array_Bound
     (Env      : Environ;
      Arr_Typ  : Entity_Id;
      Dim      : Nat;
      Is_Low   : Boolean;
      Value    : GL_Value;
      For_Type : Boolean := False) return GL_Value
   is
      Info_Idx   : constant Nat := Get_And_Create_Array_Info (Env, Arr_Typ);
      Dim_Info   : constant Index_Bounds := Array_Info.Table (Info_Idx + Dim);
      Bound_Info : constant One_Bound :=
        (if Is_Low then Dim_Info.Low else Dim_Info.High);
      Bound_Idx    : constant Nat := Dim  * 2 + (if Is_Low then 0 else 1);
      --  In the array fat pointer bounds structure, bounds are stored as a
      --  sequence of (lower bound, upper bound) pairs.

   begin

      --  There are four cases: a constant size, in which case we return
      --  that size, a saved value, in which case we return that value,
      --  an unconstrained array, in which case we have a fat pointer and
      --  extract the bounds from it, or a discriminant, in which case we
      --  access that field of the enclosing record.

      if Bound_Info.Cnst /= No_Uint then
         return Const_Int (Env, Dim_Info.Bound_Type, Bound_Info.Cnst);
      elsif Present (Bound_Info.Value) then
         return Emit_Expression (Env, Bound_Info.Value);
      elsif not Is_Constrained (Arr_Typ) then
         return G (Extract_Value
                     (Env.Bld,
                      Extract_Value (Env.Bld, Value.Value, 1, "bounds"),
                      unsigned (Bound_Idx),
                      (if Is_Low then "low-bound" else "high-bound")),
                   Dim_Info.Bound_Type);
      end if;

      --  We now should have the discriminated case.  Make sure we do.

      pragma Assert (Ekind (Bound_Info.Discr) = E_Discriminant);

      --  If we are getting the size of a type, as opposed to a value,
      --  we have to use the first/last value of the range of the type
      --  of the discriminant.

      if For_Type then
         declare
            Disc_Type : constant Entity_Id := Full_Etype (Bound_Info.Discr);
         begin
            return Emit_Expression
              (Env,
               (if Is_Low then Type_Low_Bound (Disc_Type)
               else Type_High_Bound (Disc_Type)));
         end;
      else
         return
           Load
           (Env,
            ((Record_Field_Offset
                 (Env,
                  Get_Matching_Value
                    (Full_Etype (Scope (Bound_Info.Discr))).Value,
                  Bound_Info.Discr)),
             Dim_Info.Bound_Type, Is_Reference => True));
      end if;

   end Get_Array_Bound;

   ----------------------
   -- Get_Array_Length --
   ----------------------

   function Get_Array_Length
     (Env      : Environ;
      Arr_Typ  : Entity_Id;
      Dim      : Nat;
      Value    : GL_Value;
      For_Type : Boolean := False) return GL_Value
   is
      Info_Idx    : constant Nat := Get_And_Create_Array_Info (Env, Arr_Typ);
      Dim_Info    : constant Index_Bounds := Array_Info.Table (Info_Idx + Dim);
      Low_Bound   : constant GL_Value :=
        Get_Array_Bound (Env, Arr_Typ, Dim, True, Value, For_Type);
      High_Bound  : constant GL_Value :=
        Get_Array_Bound (Env, Arr_Typ, Dim, False, Value, For_Type);
      Const_1     : constant GL_Value :=
        Const_Int (Env, Dim_Info.Bound_Type, Uint_1);
      Is_Empty    : constant GL_Value :=
        I_Cmp
        (Env,
         (if Is_Unsigned_Type (Low_Bound) then Int_UGT else Int_SGT),
         Low_Bound, High_Bound, "is-empty");
   begin
      return Build_Select
        (Env,
         C_If   => Is_Empty,
         C_Then => Const_Null (Env, Dim_Info.Bound_Type),
         C_Else =>
           (if Low_Bound = Const_1 then High_Bound
           else NSW_Add
             (Env, NSW_Sub (Env, High_Bound, Low_Bound, ""), Const_1, "")),
         Name   => "");
   end Get_Array_Length;

   --------------------------------
   -- Create_String_Literal_Type --
   --------------------------------

   function Create_String_Literal_Type
     (Env : Environ; TE : Entity_Id; Comp_Typ   : Type_T) return Type_T
   is
      First      : constant Uint :=
        Get_Uint_Value (String_Literal_Low_Bound (TE));
      Length     : constant Uint := String_Literal_Length (TE);
      Last       : constant Uint := First + Length - 1;
      Low_Bound  : constant One_Bound := (Cnst => First, Value => Empty,
                                         Discr => Empty, Dynamic => False);
      High_Bound : constant One_Bound := (Cnst => Last, Value => Empty,
                                          Discr => Empty, Dynamic => False);
      Dim_Info   : constant Index_Bounds := (Bound_Type => Standard_Positive,
                                             Low => Low_Bound,
                                             High => High_Bound);
      Result_Typ : constant Type_T :=
        Array_Type (Comp_Typ, unsigned (UI_To_Int (Length)));
   begin

      --  It's redundant to set the type here, since our caller will set it,
      --  but we have to set it in order to set the array info.

      Set_Type (Env, TE, Result_Typ);
      Array_Info.Append (Dim_Info);
      Set_Array_Info (Env, TE, Array_Info.Last);
      return Result_Typ;

   end Create_String_Literal_Type;

   -----------------------
   -- Create_Array_Type --
   -----------------------

   function Create_Array_Type
     (Env : Environ;
      TE  : Entity_Id) return Type_T
   is
      Unconstrained     : constant Boolean := not Is_Constrained (TE);
      Comp_Type         : constant Entity_Id := Component_Type (TE);
      Typ               : Type_T := Create_Type (Env, Comp_Type);
      Must_Use_Opaque   : Boolean := Is_Dynamic_Size (Env, Comp_Type);
      This_Dynamic_Size : Boolean := Must_Use_Opaque or Unconstrained;
      Index             : Entity_Id;
      Dim               : Nat := 0;
      First_Info        : constant Nat := Array_Info.Last + 1;

   begin
      if Ekind (TE) = E_String_Literal_Subtype then
         return Create_String_Literal_Type (Env, TE, Typ);
      end if;

      --  We loop through each dimension of the array creating the entries
      --  for Array_Info.  If the component type is of variable size or if
      --  either bound of an index is a dynamic size, this type is of
      --  dynamic size.  We must an opaque type if this is of dynamic size
      --  unless the only reason it's dynamic is because the first dimension
      --  is of variable-size: in that case, we can use an LLVM array with
      --  zero as the bound.

      Index := First_Index (TE);
      while Present (Index) loop
         declare
            Idx_Range : constant Node_Id := Get_Dim_Range (Index);
            --  Sometimes, the frontend leaves an identifier that
            --  references an integer subtype instead of a range.

            Index_Type : constant Entity_Id := Full_Etype (Index);
            Index_Base : constant Entity_Id :=
              Implementation_Base_Type (Index_Type);
            LB       : constant Node_Id := Low_Bound (Idx_Range);
            HB       : constant Node_Id := High_Bound (Idx_Range);
            Dim_Info : constant Index_Bounds :=
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
         Typ := Struct_Create_Named (Env.Ctx, "");
      else
         for I in reverse First_Info .. Array_Info.Last loop
            declare
               Dim_Info : constant Index_Bounds := Array_Info.Table (I);
               Low      : constant One_Bound := Dim_Info.Low;
               High     : constant One_Bound := Dim_Info.High;
               Dynamic  : constant Boolean := Low.Dynamic or else High.Dynamic;
               Rng : Long_Long_Integer := 0;
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

      Set_Type (Env, TE, Typ);
      Set_Dynamic_Size (Env, TE, This_Dynamic_Size);
      Set_Array_Info (Env, TE, First_Info);

      return Typ;
   end Create_Array_Type;

   ------------------------------
   -- Create_Array_Bounds_Type --
   ------------------------------

   function Create_Array_Bounds_Type
     (Env             : Environ;
      Array_Type_Node : Entity_Id) return Type_T
   is
      Dims       : constant Nat := Number_Dimensions (Array_Type_Node);
      Fields     : aliased array (Nat range 0 .. 2 * Dims - 1) of Type_T;
      First_Info : constant Nat :=
        Get_And_Create_Array_Info (Env, Array_Type_Node);
      J          : Nat := 0;
   begin
      for I in Nat range 0 .. Dims - 1 loop
         Fields (J) :=
           Create_Type (Env, Array_Info.Table (First_Info + I).Bound_Type);
         Fields (J + 1) := Fields (J);
         J := J + 2;
      end loop;

      return Struct_Type_In_Context
        (Env.Ctx, Fields'Address, Fields'Length, Packed => False);
   end Create_Array_Bounds_Type;

   -----------------------------------
   -- Create_Array_Raw_Pointer_Type --
   -----------------------------------

   function Create_Array_Raw_Pointer_Type
     (Env             : Environ;
      Array_Type_Node : Entity_Id) return Type_T
   is
      Elt_Type : constant Type_T :=
        Create_Type (Env, Component_Type (Array_Type_Node));
      Arr_Type : constant Type_T := Array_Type (Elt_Type, 0);
   begin
      return Pointer_Type (Arr_Type, 0);
   end Create_Array_Raw_Pointer_Type;

   -----------------------------------
   -- Create_Array_Fat_Pointer_Type --
   -----------------------------------

   function Create_Array_Fat_Pointer_Type
     (Env        : Environ;
      Array_Type : Entity_Id) return Type_T
   is
      St_Els : Type_Array (1 .. 2) :=
        (Create_Array_Raw_Pointer_Type (Env, Array_Type),
         Create_Array_Bounds_Type (Env, Array_Type));
   begin
      return Struct_Type (St_Els'Address, St_Els'Length, False);
   end Create_Array_Fat_Pointer_Type;

   --------------------
   -- Get_Array_Size --
   --------------------

   function Get_Array_Size
     (Env         : Environ;
      Array_Descr : Value_T;
      Array_Type  : Entity_Id;
      For_Type    : Boolean := False) return Value_T
   is
      Size : Value_T := Const_Int (Env.Size_Type, 1, Sign_Extend => False);
   begin

      --  Go through every array dimension.  Get its size and multiply all
      --  of them together.

      for Dim in Nat range 0 .. Number_Dimensions (Array_Type) - 1 loop
         Size :=
           NSW_Mul (Env.Bld, Size,
                    Convert_To_Size_Type
                      (Env, Get_Array_Length
                         (Env, Array_Type, Dim,
                          (if No (Array_Descr) then No_GL_Value
                           else G (Array_Descr, Array_Type)),
                             For_Type).Value),
                    "");
      end loop;

      return Size;
   end Get_Array_Size;

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
      Fat_Ptr           : Value_T := Get_Undef (Fat_Ptr_Type);
      Array_Data_Ptr    : Value_T;
      Bounds            : Value_T;

   begin
      pragma Assert (Count_Struct_Element_Types (Fat_Ptr_Type) = 2);
      Get_Struct_Element_Types (Fat_Ptr_Type, Fat_Ptr_Elt_Types'Address);

      Array_Data_Ptr :=
        Pointer_Cast (Env.Bld, Array_Data, Array_Data_Type, "");
      Bounds := Get_Undef (Array_Bounds_Type);

      --  ??  This may be a kludge until we figure out a consistent
      --  way of elaborating all types.

      Discard (Create_Type (Env, Array_Type));

      for Dim in Nat range 0 .. Number_Dimensions (Array_Type) - 1 loop
         Bounds := Insert_Value
           (Env.Bld,
            Bounds,
            Get_Array_Bound (Env, Array_Type, Dim, True,
                             G (Array_Data, Array_Type)).Value,
            unsigned (Dim * 2),
            "");

         Bounds := Insert_Value
           (Env.Bld,
            Bounds,
            Get_Array_Bound (Env, Array_Type, Dim, False,
                             G (Array_Data, Array_Type)).Value,
            unsigned (Dim * 2 + 1),
            "");
      end loop;

      --  Then fill the fat pointer itself
      Fat_Ptr := Insert_Value (Env.Bld, Fat_Ptr, Array_Data_Ptr, 0, "");
      Fat_Ptr := Insert_Value (Env.Bld, Fat_Ptr, Bounds, 1, "");

      return Fat_Ptr;
   end Array_Fat_Pointer;

   -------------------
   -- Array_Address --
   -------------------

   function Array_Address
     (Env        : Environ;
      Array_Data : Value_T;
      Array_Type : Entity_Id) return Value_T
   is
      Idx_Type : constant Type_T :=
        Create_Type (Env, Full_Etype (First_Index (Array_Type)));
      Zero     : constant Value_T := Const_Null (Idx_Type);
      Idx      : constant Value_Array (0 .. Number_Dimensions (Array_Type)) :=
        (0 => Const_Null (Intptr_T), others => Zero);

   begin
      return GEP (Env.Bld, Array_Data, Idx, "array-addr");
   end Array_Address;

   ----------------------------------
   -- Get_Innermost_Component_Type --
   ----------------------------------

   function Get_Innermost_Component_Type
     (Env : Environ; N : Entity_Id) return Type_T
   is
     (if Is_Array_Type (N)
      then Get_Innermost_Component_Type (Env, Component_Type (N))
      else Create_Type (Env, N));

   ------------------------
   -- Dynamic_Size_Array --
   ------------------------

   function Dynamic_Size_Array (T : Entity_Id) return Boolean is
      Indx : Node_Id;
      Ityp : Entity_Id;

   begin
      if not Is_Array_Type (T) then
         return False;
      end if;

      --  Loop to process array indexes

      Indx := First_Index (T);
      while Present (Indx) loop
         Ityp := Full_Etype (Indx);

         --  If an index of the array is a generic formal type then there is
         --  no point in determining a size for the array type.

         if Is_Generic_Type (Ityp) then
            return False;
         end if;

         if not Compile_Time_Known_Value (Type_Low_Bound (Ityp))
           or else not Compile_Time_Known_Value (Type_High_Bound (Ityp))
         then
            return True;
         end if;

         Next_Index (Indx);
      end loop;

      return False;
   end Dynamic_Size_Array;

   ------------------------
   -- Get_Indexed_LValue --
   ------------------------
   function Get_Indexed_LValue
     (Env     : Environ;
      Arr_Typ : Entity_Id;
      Indexes : List_Id;
      Value   : Value_T) return Value_T
   is
      Array_Data_Ptr : constant Value_T := Array_Data (Env, Value, Arr_Typ);
      LLVM_Array_Typ : constant Type_T := Create_Type (Env, Arr_Typ);
      Idxs : Value_Array (1 .. List_Length (Indexes) + 1) :=
        (1 => Const_Int (Intptr_T, 0, Sign_Extend => False), others => <>);
      --  Operands for the GetElementPtr instruction: one for the
      --  pointer deference, and then one per array index.

      J : Nat := 2;
      N : Node_Id;
   begin
      N := First (Indexes);
      while Present (N) loop
         --  Adjust the index according to the range lower bound

         declare
            User_Index    : constant Value_T := Emit_Expression (Env, N).Value;
            Dim_Low_Bound : constant Value_T :=
              Get_Array_Bound (Env, Arr_Typ, J - 2, True,
                               G (Value, Arr_Typ)).Value;
         begin
            Idxs (J) := NSW_Sub (Env.Bld, User_Index, Dim_Low_Bound, "index");
         end;

         J := J + 1;
         N := Next (N);
      end loop;

      --  There are two approaches we can take here.  If we haven't used
      --  an opaque type, we can just do a GEP with the values above.

      if Type_Is_Sized (LLVM_Array_Typ) then
         return GEP (Env.Bld, Array_Data_Ptr, Idxs, "array-element-access");
      end if;

      --  Otherwise, we convert the array data type to an i8*, compute the
      --  byte offset from the index and size information, index that, and
      --  then convert back to the array type.  We start with the first
      --  index then for each dimension after the first, multiply by the
      --  size of that dimension and add that index.  Finally, we multiply
      --  by the size of the component.  We do all of this in Size_Type.

      declare
         Int8_Ptr      : constant Type_T := Pointer_Type (Int_Ty (8), 0);
         Data          : constant Value_T :=
           Bit_Cast (Env.Bld, Array_Data_Ptr, Int8_Ptr, "");
         Comp_Type     : constant Entity_Id :=
           Get_Fullest_View (Component_Type (Arr_Typ));
         LLVM_Comp_Typ : constant Type_T := Create_Type (Env, Comp_Type);
         Comp_Size     : constant Value_T :=
           Get_Type_Size (Env, LLVM_Comp_Typ, Comp_Type, No_Value_T);
         Index         : Value_T := Convert_To_Size_Type (Env, Idxs (2));
      begin

         for Dim in 1 .. Number_Dimensions (Arr_Typ) - 1 loop
            Index := NSW_Add (Env.Bld,
                              NSW_Mul (Env.Bld,
                                       Index,
                                       Convert_To_Size_Type
                                         (Env,
                                          Get_Array_Length
                                            (Env, Arr_Typ, Dim,
                                             G (Array_Data_Ptr,
                                                Arr_Typ)).Value),
                                       ""),
                              Convert_To_Size_Type (Env, Idxs (Dim + 2)),
                              "");
         end loop;

         Index := NSW_Mul (Env.Bld, Index, Comp_Size, "");
         return Bit_Cast
           (Env.Bld, GEP (Env.Bld, Data, (1 => Index), "gen-index"),
            Pointer_Type (LLVM_Array_Typ, 0), "");
      end;

   end Get_Indexed_LValue;

   ----------------------
   -- Get_Slice_LValue --
   ----------------------

   function Get_Slice_LValue
     (Env         : Environ;
      Arr_Typ     : Entity_Id;
      Result_Type : Entity_Id;
      Rng         : Node_Id;
      Value       : Value_T) return Value_T
     is
      Array_Data_Ptr : constant Value_T := Array_Data (Env, Value, Arr_Typ);
      LLVM_Array_Typ : constant Type_T := Create_Type (Env, Arr_Typ);

      --  Compute how much we need to offset the array pointer. Slices
      --  can be built only on single-dimension arrays

      Index_Shift : constant Value_T :=
        Sub
        (Env.Bld, Emit_Expression (Env, Low_Bound (Rng)).Value,
         Get_Array_Bound (Env, Arr_Typ, 0, True,
                          G (Value, Arr_Typ)).Value, "offset");
   begin

      --  Like the above case, we have to hande both the opaque and non-opaque
      --  cases.  Luckily, we know we're only a single dimension.

      if Type_Is_Sized (LLVM_Array_Typ) then
         return Bit_Cast
           (Env.Bld,
            GEP
              (Env.Bld,
               Array_Data_Ptr,
               (Const_Int (Intptr_T, 0, Sign_Extend => False), Index_Shift),
               "array-shifted"),
            Create_Access_Type (Env, Result_Type), "slice");
      end if;

      declare
         Int8_Ptr      : constant Type_T := Pointer_Type (Int_Ty (8), 0);
         Data          : constant Value_T :=
           Bit_Cast (Env.Bld, Array_Data_Ptr, Int8_Ptr, "");
         Comp_Type     : constant Entity_Id :=
           Get_Fullest_View (Component_Type (Arr_Typ));
         LLVM_Comp_Typ : constant Type_T := Create_Type (Env, Comp_Type);
         Comp_Size     : constant Value_T :=
           Get_Type_Size (Env, LLVM_Comp_Typ, Comp_Type, No_Value_T);
         Index         : constant Value_T :=
           NSW_Mul (Env.Bld, Comp_Size,
                    Convert_To_Size_Type (Env, Index_Shift),
                    "");
      begin
         return Bit_Cast
           (Env.Bld, GEP (Env.Bld, Data, (1 => Index), "gen-index"),
            Pointer_Type (LLVM_Array_Typ, 0), "");
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
