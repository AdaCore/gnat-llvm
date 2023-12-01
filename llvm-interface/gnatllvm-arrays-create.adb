------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2023, AdaCore                     --
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

with Sem_Eval; use Sem_Eval;
with Snames;   use Snames;
with Table;    use Table;

with GNATLLVM.Compile;      use GNATLLVM.Compile;
with GNATLLVM.Instructions; use GNATLLVM.Instructions;
with GNATLLVM.Types.Create; use GNATLLVM.Types.Create;
with GNATLLVM.Utils;        use GNATLLVM.Utils;
with GNATLLVM.Variables;    use GNATLLVM.Variables;

package body GNATLLVM.Arrays.Create is

   function Known_Not_Superflat (N : N_Has_Bounds_Id) return Boolean;
   --  Return True if the range described by N is known not to be
   --  able to be superflat.

   function FLB_Known_Not_Superflat (N : N_Is_Index_Id) return Boolean;
   --  Likewise, but for an array with a fixed lower bound

   function Build_One_Bound
     (N             : N_Subexpr_Id;
      Unconstrained : Boolean;
      For_Orig      : Boolean) return One_Bound;
   --  Helper function to build a One_Bound object from N

   function Create_String_Literal_Type
     (TE : E_String_Literal_Subtype_Id; Comp_Typ : Type_T) return Type_T
     with Pre  => Ekind (TE) = E_String_Literal_Subtype
                  and then Present (Comp_Typ),
          Post => (Get_Type_Kind (Create_String_Literal_Type'Result) =
                     Array_Type_Kind);
   --  Helper function to create type for string literals

   --  For each array type, we record the types built for the bounds, bounds
   --  and data, and fat pointer. We have a table of these and an GL_Type
   --  can optionally point to that.

   type Array_Types_Data is record
     Bounds          : Type_T;
     Bounds_And_Data : Type_T;
     Fat_Pointer     : Type_T;
   end record;

   package Array_Types is new Table.Table
     (Table_Component_Type => Array_Types_Data,
      Table_Index_Type     => Array_Types_Id,
      Table_Low_Bound      => Array_Types_Low_Bound,
      Table_Initial        => 100,
      Table_Increment      => 50,
      Table_Name           => "Array_Types");

   --  Define setter and getter subprograms for the above table, but starting
   --  with a GT in all cases.

   function Get_Or_Create_Array_Types
     (GT : Array_Or_PAT_GL_Type) return Array_Types_Id
     with Post => Present (Get_Or_Create_Array_Types'Result);
   --  If we've already made an Array_Types entry for GT, return it.
   --  Otherwise, make a new one.

   function Get_Bounds_Type           (GT : Array_Or_PAT_GL_Type) return Type_T
     with Inline;
   function Get_Bounds_And_Data_Type  (GT : Array_Or_PAT_GL_Type) return Type_T
     with Inline;
   function Get_Fat_Pointer_Type      (GT : Array_Or_PAT_GL_Type) return Type_T
     with Inline;

   procedure Set_Bounds_Type          (GT : Array_Or_PAT_GL_Type; T : Type_T)
     with Pre => Present (T), Inline;
   procedure Set_Bounds_And_Data_Type (GT : Array_Or_PAT_GL_Type; T : Type_T)
     with Pre => Present (T), Inline;
   procedure Set_Fat_Pointer_Type     (GT : Array_Or_PAT_GL_Type; T : Type_T)
     with Pre => Present (T), Inline;

   function Create_Array_Fat_Pointer_Type_Internal
     (GT : Array_Or_PAT_GL_Type) return Type_T
     with Post => Present (Create_Array_Fat_Pointer_Type_Internal'Result);
   --  Return the type used for fat pointers to the array type GT

   function Create_Array_Bounds_Type_Internal
     (GT : Array_Or_PAT_GL_Type) return Type_T
     with Post => Present (Create_Array_Bounds_Type_Internal'Result);
   --  Return the type used to store array bounds. This is a structure
   --  that that follows the following pattern: { LB0, UB0, LB1, UB1, ... }

   function Create_Array_Bounds_And_Data_Type_Internal
     (GT : Array_Or_PAT_GL_Type) return Type_T
     with Post => Present (Create_Array_Bounds_And_Data_Type_Internal'Result);
   --  Return the type used to store the bounds and data of an array

   -------------------------
   -- Known_Not_Superflat --
   -------------------------

   function Known_Not_Superflat (N : N_Has_Bounds_Id) return Boolean is
      LB      : N_Subexpr_Id := Low_Bound  (N);
      HB      : N_Subexpr_Id := High_Bound (N);
      TE      : Type_Kind_Id;

   begin
      --  This is the easy case

      if Nkind (N) = N_Range and then Cannot_Be_Superflat (N) then
         return True;
      end if;

      --  If the low bound is not constant, see if we can find the upper
      --  bound of the subtype of the lower bound, since that's the
      --  worse case. Do this repeatedly, if need be.

      loop
         exit when Nkind (LB) = N_Integer_Literal;
         TE := Full_Etype (LB);
         exit when Ekind (TE) not in Integer_Kind or else Is_Base_Type (TE);
         LB := High_Bound (Simplify_Range (Scalar_Range (TE)));
      end loop;

      --  Similarly for the high bound

      loop
         exit when Nkind (HB) = N_Integer_Literal;
         TE := Full_Etype (HB);

         exit when Ekind (TE) not in Integer_Kind or else Is_Base_Type (TE);
         HB := Low_Bound (Simplify_Range (Scalar_Range (TE)));
      end loop;

      --  If both are integers and the bounds are safe, we can't be superflat

      return Nkind (LB) = N_Integer_Literal
        and then Nkind (HB) = N_Integer_Literal
        and then Intval (HB) >= Intval (LB) - 1;
   end Known_Not_Superflat;

   -----------------------------
   -- FLB_Known_Not_Superflat --
   -----------------------------

   function FLB_Known_Not_Superflat (N : N_Is_Index_Id) return Boolean is
      Our_Rng    : constant N_Has_Bounds_Id := Simplify_Range (N);
      Parent_Rng : N_Has_Bounds_Id;
      Parent_LB  : N_Subexpr_Id;

   begin
      --  Start with the easy case

      if Nkind (N) = N_Range and then Cannot_Be_Superflat (N) then
         return True;

      --  If this is forming a subtype where our parent subtype's lower
      --  bound is also an integer and our lower bound minus one is no
      --  greater than that bound, we can't form superflat objects.

      elsif Nkind (N) = N_Subtype_Indication then
         Parent_Rng := Simplify_Range (N);
         Parent_LB  := Low_Bound (Parent_Rng);
         return Nkind (Parent_LB) = N_Integer_Literal
           and then Intval (Low_Bound (Our_Rng)) - 1 <= Intval (Parent_LB);
      else
         return False;
      end if;
   end FLB_Known_Not_Superflat;

   ---------------------
   -- Build_One_Bound --
   ---------------------

   function Build_One_Bound
     (N             : N_Subexpr_Id;
      Unconstrained : Boolean;
      For_Orig      : Boolean) return One_Bound
   is
      Val : Uint;

   begin
      --  If this is an unconstrained array, indicate so

      if Unconstrained then
         return (Cnst => No_Uint, Value => Empty);

      --  If this is a constant known to the front end, use that constant.
      --  In case the constant is an Enum, use the representation value
      --  for the original array type, otherwise use the enum value.

      elsif Compile_Time_Known_Value (N) then
         Val := (if For_Orig then Expr_Rep_Value (N) else Expr_Value (N));
         return (Cnst => Val, Value => Empty);

      --  Even if this isn't a constant known to the front end, see if we
      --  can evaluate it at compile-time (without generating any code).
      --  If so, see if that results in an integer (it might be a symbolic
      --  value) and an integer that's in range of an Int. If all that is
      --  true, make a Uint out of it and use it as a constant bound.

      elsif Is_No_Elab_Needed (N) then
         declare
            V   : constant GL_Value := Emit_Expression (N);
            Val : LLI;

         begin
            if Is_A_Constant_Int (V) then
               Val := +V;

               if Val in LLI (Int'First) .. LLI (Int'Last) then
                  return (Cnst => +Val, Value => Empty);
               end if;
            end if;
         end;
      end if;

      --  If we reach here, this must be a dynamic case

      return (Cnst => No_Uint, Value => N);

   end Build_One_Bound;

   --------------------------------
   -- Create_String_Literal_Type --
   --------------------------------

   function Create_String_Literal_Type
     (TE : E_String_Literal_Subtype_Id; Comp_Typ : Type_T) return Type_T
   is
      First      : constant Uint         :=
        Get_Uint_Value (String_Literal_Low_Bound (TE));
      Length     : constant Uint         := String_Literal_Length (TE);
      Last       : constant Uint         := First + Length - 1;
      Low_Bound  : constant One_Bound    := (Cnst => First, Value => Empty);
      High_Bound : constant One_Bound    := (Cnst => Last, Value => Empty);
      Index_TE   : constant Type_Kind_Id :=
        Full_Etype (First_Index (Full_Base_Type (TE)));
      Dim_Info   : constant Index_Bounds :=
        (Bound_GT      => Base_GL_Type (Index_TE),
         Bound_Sub_GT  => Primitive_GL_Type (Index_TE),
         Low           => Low_Bound,
         High          => High_Bound,
         Bound_Range   => Size_Const_Int (Length),
         First_Field   => 0,
         Not_Superflat => True);
      Result_Typ : constant Type_T       :=
        Array_Type (Comp_Typ, unsigned (+Length));

   begin
      Array_Info.Append (Dim_Info);
      Set_Array_Info (TE, Array_Info.Last);
      return Result_Typ;

   end Create_String_Literal_Type;

   -----------------------
   -- Create_Array_Type --
   -----------------------

   function Create_Array_Type
     (TE : Type_Kind_Id; For_Orig : Boolean := False) return Type_T
   is
      type Dim_Info_Array is array (Nat range <>) of Index_Bounds;

      A_TE              : constant Type_Kind_Id :=
        (if For_Orig then Full_Original_Array_Type (TE) else TE);
      Unconstrained     : constant Boolean      := not Is_Constrained (A_TE);
      CT                : constant Type_Kind_Id := Full_Component_Type (A_TE);
      Comp_Def_GT       : constant GL_Type      := Default_GL_Type (CT);
      Comp_Size_To_Use  : constant Uint         :=
        (if    Known_Static_Component_Size (A_TE) and then not For_Orig
         then  Component_Size (A_TE)
         elsif Is_Packed (A_TE) and then not Is_Packed_Array_Impl_Type (TE)
               and then not Strict_Alignment (Comp_Def_GT)
         then  RM_Size (Comp_Def_GT) else No_Uint);
      Comp_Size         : constant Uint         :=
         Validate_Size (A_TE, Comp_Def_GT, Comp_Size_To_Use,
                        For_Component => True,
                        Zero_Allowed  => Has_Component_Size_Clause (A_TE));
      Max_Size          : constant Boolean      :=
        Is_Unconstrained_Record (Comp_Def_GT);
      Biased            : constant Boolean      :=
        Has_Biased_Representation (A_TE);
      Comp_Initial_GT   : constant GL_Type      :=
        Make_GT_Alternative (Comp_Def_GT, TE,
                             Size          => Comp_Size,
                             For_Component => True,
                             Max_Size      => Max_Size,
                             Is_Biased     => Biased);
      Comp_GT           : constant GL_Type      :=
        (if  Has_Aliased_Components (A_TE)
             and then Present (GT_Size (Comp_Initial_GT))
             and then Is_Const_Int_Value (GT_Size (Comp_Initial_GT), 0)
         then Make_GT_Alternative (Comp_Initial_GT, TE,
                                   Size          => +BPU,
                                   For_Component => True)
         else Comp_Initial_GT);
      Base_Type         : constant Type_Kind_Id :=
        Full_Base_Type (A_TE, For_Orig);
      Must_Use_Fake     : Boolean               :=
        not Is_Native_Component_GT (Comp_GT);
      This_Nonnative    : Boolean               :=
        Must_Use_Fake or Unconstrained;
      CT_To_Use         : constant GL_Type      :=
        (if Must_Use_Fake then SSI_GL_Type else Comp_GT);
      Typ               : Type_T                := Type_Of (CT_To_Use);
      Dim               : Nat                   := 0;
      Last_Dim          : constant Nat          :=
        (if   Ekind (A_TE) = E_String_Literal_Subtype
         then 1 else Number_Dimensions (A_TE) - 1);
      Total_Size        : GL_Value              :=
        (if This_Nonnative then Size_Const_Null else Get_Type_Size (Comp_GT));
      Field_Index       : Nat                   := 0;
      Dim_Infos         : Dim_Info_Array (0 .. Last_Dim);
      First_Info        : Array_Info_Id;
      Index             : Opt_N_Is_Index_Id;
      Base_Index        : Opt_N_Is_Index_Id;

   begin
      --  String literal subtypes are simple, so handle them separately

      if Ekind (A_TE) = E_String_Literal_Subtype then
         Set_Associated_GL_Type (A_TE, Comp_GT);
         return Create_String_Literal_Type (A_TE, Typ);
      end if;

      --  Set the associated component type unless doing this for an
      --  original type of a packed array type.

      if not For_Orig then
         Set_Associated_GL_Type (A_TE, Comp_GT);
      end if;

      --  Check for an array that requires atomic components

      if  Has_Atomic_Components (A_TE) or else Is_Full_Access (Comp_GT) then
         Check_OK_For_Atomic_Type (Comp_GT, A_TE, True);
      end if;

      --  If this is a base type, back-annotate the component size

      if Is_Base_Type (A_TE) and then not Known_Component_Size (A_TE) then
         Set_Component_Size (A_TE, Annotated_Object_Size (Comp_GT));
      end if;

      --  We loop through each dimension of the array creating the entries
      --  for Array_Info. If the component type is of variable size or if
      --  either bound of an index is a dynamic size, this type is of
      --  dynamic size. We could use an opaque type in that case, but we
      --  have numerous array subtypes that should be treated identically
      --  but couldn't if we took that approach. However, all of those
      --  subtypes will have the same component type. If that component
      --  type is of fixed size, we can make an LLVM array [0 x CT] where
      --  CT is the component type. Otherwise, we have to use [0 x i8]. We
      --  refer to both of these cases as creating a "fake" type.

      Index      := First_Index (A_TE);
      Base_Index := First_Index (Base_Type);
      while Present (Index) loop
         declare
            Idx_Range : constant N_Has_Bounds_Id := Simplify_Range (Index);
            --  Sometimes, the frontend leaves an identifier that
            --  references an integer subtype instead of a range.

            FLB               : constant Boolean :=
              Nkind (Index) = N_Subtype_Indication
              and then Is_Fixed_Lower_Bound_Index_Subtype (Etype (Index));
            Index_GT  : constant GL_Type         := Full_GL_Type (Index);
            Index_BT  : constant GL_Type         := Base_GL_Type (Index_GT);
            LB        : constant N_Subexpr_Id    := Low_Bound  (Idx_Range);
            HB        : constant N_Subexpr_Id    := High_Bound (Idx_Range);
            Dim_Info  : Index_Bounds     :=
              (Bound_GT      => Index_BT,
               Bound_Sub_GT  => Full_GL_Type (Base_Index),
               Low           =>
                 Build_One_Bound (LB, Unconstrained and not FLB, For_Orig),
               High          => Build_One_Bound (HB, Unconstrained, For_Orig),
               Bound_Range   => No_GL_Value,
               First_Field   => Field_Index,
               Not_Superflat =>
                 (Ekind (TE) = E_Array_Subtype and then Nkind (Index) = N_Range
                    and then Is_Constrained (TE)
                    and then Known_Not_Superflat (Idx_Range))
                 or else (not For_Orig and then Is_Packed_Array_Impl_Type (TE)
                            and then Is_Bit_Packed_Array
                                       (Original_Array_Type (TE)))
                 or else (FLB and then FLB_Known_Not_Superflat (Index)));

            --  We have to be careful here and flag the type of the index
            --  from that of the base type since we can have index ranges
            --  that are outside the base type if the subtype is superflat
            --  (see C37172C). We also need to record the subtype of the
            --  index as it appears in the base array type since that's
            --  what's used to compute the min/max sizes of objects.
            LB_Uint   : constant Uint    := Dim_Info.Low.Cnst;
            HB_Uint   : constant Uint    := Dim_Info.High.Cnst;
            Idx_Const : constant Boolean :=
              Present (LB_Uint) and then Present (HB_Uint)
              and then UI_Is_In_Int_Range (HB_Uint - LB_Uint + 1);

         begin
            --  Update whether or not this will be of dynamic size and
            --  whether we must use a fake type based on this dimension.
            --  Then record it.

            if Idx_Const then
               Dim_Info.Bound_Range :=
                 Bounds_To_Length (Size_Const_Int (Dim_Info.Low.Cnst),
                                   Size_Const_Int (Dim_Info.High.Cnst),
                                   Size_GL_Type, Dim_Info.Not_Superflat);
               Total_Size := Total_Size * Dim_Info.Bound_Range;
            else
               This_Nonnative := True;

               if Dim /= 0 then
                  Must_Use_Fake := True;
               end if;
            end if;

            Dim_Infos (Dim) := Dim_Info;
            Dim             := Dim + 1;
            Field_Index     := Field_Index + (if FLB then 1 else 2);
            Next_Index (Index);
            Next_Index (Base_Index);
         end;
      end loop;

      --  Now write all the dimension information into the array table. We
      --  do it here in case we elaborate any types above.

      First_Info := Array_Info.Last + Nat (1);
      for J in Dim_Infos'Range loop
         Array_Info.Append (Dim_Infos (J));
      end loop;

      --  If the total size of this type is bigger than we can fit in an
      --  Int, use a nonnative type.

      if not This_Nonnative
        and then (Overflowed (Total_Size) or else +Total_Size > LLI (Int'Last))
      then
         This_Nonnative := True;
      end if;

      --  If not using a native types, then make a type with a zero
      --  number of elements and the type we set above. Otherwise loop
      --  through the types making the LLVM type.

      if This_Nonnative then
         Typ := Array_Type (Typ, 0);
      else
         for J in reverse First_Info .. Array_Info.Last loop
            declare
               Idx : constant Array_Info_Id :=
                 (if   Convention (TE) = Convention_Fortran
                  then Array_Info.Last + First_Info - J else J);
               Rng : constant GL_Value      :=
                 Array_Info.Table (Idx).Bound_Range;

            begin
               Typ := Array_Type (Typ,
                                  (if   Present (Rng)
                                   then unsigned (Get_Const_Int_Value (Rng))
                                   else 0));
            end;
         end loop;
      end if;

      --  Now set our results, either recording it as the information for
      --  the original array type or as the primary info. In the latter
      --  case, we do a redundant-looking setting of the type to simplify
      --  handling of the other sets.

      if For_Orig then
         Set_Orig_Array_Info   (TE, First_Info);
      else
         Set_Is_Nonnative_Type (TE, This_Nonnative);
         Set_Array_Info        (TE, First_Info);
      end if;

      return Typ;
   end Create_Array_Type;

   -------------------------------
   -- Get_Or_Create_Array_Types --
   -------------------------------

   function Get_Or_Create_Array_Types
     (GT : Array_Or_PAT_GL_Type) return Array_Types_Id
   is
      ATs : Array_Types_Id := Get_Array_Types (GT);

   begin
      --  If we didn't previously make an entry, make an empty one now

      if No (ATs) then
         Array_Types.Append ((No_Type_T, No_Type_T, No_Type_T));
         ATs := Array_Types.Last;
         Set_Array_Types (GT, ATs);
      end if;

      return ATs;
   end Get_Or_Create_Array_Types;

   ---------------------
   -- Get_Bounds_Type --
   ---------------------

   function Get_Bounds_Type (GT : Array_Or_PAT_GL_Type) return Type_T is
      ATs : constant Array_Types_Id := Get_Array_Types (GT);
   begin
      return (if   Present (ATs) then Array_Types.Table (ATs).Bounds
              else No_Type_T);
   end Get_Bounds_Type;

   ------------------------------
   -- Get_Bounds_And_Data_Type --
   ------------------------------

   function Get_Bounds_And_Data_Type (GT : Array_Or_PAT_GL_Type) return Type_T
   is
      ATs : constant Array_Types_Id := Get_Array_Types (GT);
   begin
      return (if   Present (ATs)
              then Array_Types.Table (ATs).Bounds_And_Data
              else No_Type_T);
   end Get_Bounds_And_Data_Type;

   --------------------------
   -- Get_Fat_Pointer_Type --
   --------------------------

   function Get_Fat_Pointer_Type (GT : Array_Or_PAT_GL_Type) return Type_T is
      ATs : constant Array_Types_Id := Get_Array_Types (GT);
   begin
      return (if   Present (ATs) then Array_Types.Table (ATs).Fat_Pointer
              else No_Type_T);
   end Get_Fat_Pointer_Type;

   ---------------------
   -- Set_Bounds_Type --
   ---------------------

   procedure Set_Bounds_Type (GT : Array_Or_PAT_GL_Type; T : Type_T) is
      ATs : constant Array_Types_Id := Get_Or_Create_Array_Types (GT);
   begin
      Array_Types.Table (ATs).Bounds := T;
   end Set_Bounds_Type;

   ------------------------------
   -- Set_Bounds_And_Data_Type --
   ------------------------------

   procedure Set_Bounds_And_Data_Type (GT : Array_Or_PAT_GL_Type; T : Type_T)
   is
      ATs : constant Array_Types_Id := Get_Or_Create_Array_Types (GT);
   begin
      Array_Types.Table (ATs).Bounds_And_Data := T;
   end Set_Bounds_And_Data_Type;

   --------------------------
   -- Set_Fat_Pointer_Type --
   --------------------------

   procedure Set_Fat_Pointer_Type (GT : Array_Or_PAT_GL_Type; T : Type_T) is
      ATs : constant Array_Types_Id := Get_Or_Create_Array_Types (GT);
   begin
      Array_Types.Table (ATs).Fat_Pointer := T;
   end Set_Fat_Pointer_Type;

   ---------------------------------------
   -- Create_Array_Bounds_Type_Internal --
   ---------------------------------------

   function Create_Array_Bounds_Type_Internal
     (GT : Array_Or_PAT_GL_Type) return Type_T
   is
      A_GT       : constant Array_Kind_Id :=
        (if   Is_Packed_Array_Impl_Type (GT) then Full_Original_Array_Type (GT)
         else Full_Etype (GT));
      Dims       : constant Nat           := Number_Dimensions (A_GT);
      Bounds     : constant Nat           := Number_Bounds (A_GT);
      Fields     : aliased Type_Array (Nat range 0 .. Bounds - 1);
      F_Names    : Name_Id_Array (0 .. Bounds - 1);
      First_Info : constant Array_Info_Id :=
        (if   Is_Packed_Array_Impl_Type (GT)
         then Get_Orig_Array_Info (Full_Etype (GT))
         else Get_Array_Info (Full_Etype (GT)));
      Bound_Idx  : Nat                    := 0;

   begin
      --  If we just have one bound (which is a single-dimensioned array
      --  with a fixed lower bound, we can use the type of that bound.

      if Bounds = 1 then
         return Type_Of (Array_Info.Table (First_Info).Bound_Sub_GT);
      end if;

      --  Otherwise, create a struct with all of the bounds

      for J in Nat range 0 .. Dims - 1 loop
         declare
            IB : constant Index_Bounds := Array_Info.Table (First_Info + J);

         begin
            --  Add the type and name of the fields representing the bounds.
            --  We always have an upper bound, but don't have a lower bound
            --  if it's fixed.

            if not Is_FLB (IB) then
               Fields  (Bound_Idx) := Type_Of (IB.Bound_Sub_GT);
               F_Names (Bound_Idx) := Name_Find ("LB" & To_String (J));
               Bound_Idx           := Bound_Idx + 1;
            end if;

            Fields  (Bound_Idx)    := Type_Of (IB.Bound_Sub_GT);
            F_Names (Bound_Idx)    := Name_Find ("UB" & To_String (J));
            Bound_Idx              := Bound_Idx + 1;
         end;
      end loop;

      return Build_Struct_Type (Fields,
                                Name        => Get_Ext_Name (GT, "_BOUNDS"),
                                Field_Names => F_Names);
   end Create_Array_Bounds_Type_Internal;

   ------------------------------------------------
   -- Create_Array_Bounds_And_Data_Type_Internal --
   ------------------------------------------------

   function Create_Array_Bounds_And_Data_Type_Internal
     (GT : Array_Or_PAT_GL_Type) return Type_T
   is
      Align  : constant Nat    := Get_Type_Alignment (GT);
      B_T    : constant Type_T := Create_Array_Bounds_Type (GT);
      B_T_Sz : constant Nat    := Nat (ULL'(Get_Type_Size (B_T)));

   begin
      --  If the size of the bounds type is a multiple of the alignment,
      --  we have the normal case of two types.

      if B_T_Sz mod Align = 0 then
         return Build_Struct_Type ((1 => B_T, 2 => Type_Of (GT)),
                                   Name        => Get_Ext_Name (GT, "_BD"),
                                   Field_Names => (1 => Name_Find ("BOUNDS"),
                                                   2 => Name_Find ("DATA")));

      --  Otherwise, generate some padding

      else
         declare
            Align_Sz : constant Nat := (B_T_Sz + Align - 1) / Align * Align;
            Pad      : constant Nat := (Align_Sz - B_T_Sz) / BPU;

         begin
            return
              Build_Struct_Type ((1 => B_T,
                                  2 => Array_Type (Byte_T, unsigned (Pad)),
                                  3 => Type_Of (GT)),
                                 Name => Get_Ext_Name (GT, "_BD"),
                                 Field_Names => (1 => Name_Find ("BOUNDS"),
                                                 2 => No_Name,
                                                 3 => Name_Find ("DATA")));

         end;
      end if;
   end Create_Array_Bounds_And_Data_Type_Internal;

   --------------------------------------------
   -- Create_Array_Fat_Pointer_Type_Internal --
   --------------------------------------------

   function Create_Array_Fat_Pointer_Type_Internal
     (GT : Array_Or_PAT_GL_Type) return Type_T
   is
      Name      : constant Name_Id := Get_Ext_Name (GT, "_FP");
      P_Data_T  : constant Type_T :=
        Pointer_Type (Type_Of (GT), Address_Space);
      Data_Name : constant Name_Id := Name_Find ("P_DATA");

   begin
      --  A fat pointer is normally a pointer to the data and a pointer to
      --  the bounds, but in some cases contains the bound itself.

      if Has_Bounds_In_Fat_Pointer (GT) then
         return Build_Struct_Type
           ((1 => P_Data_T, 2 => Create_Array_Bounds_Type (GT)),
            Name        => Name,
            Field_Names => (0 => Data_Name, 1 => Name_Find ("UB")));
      else
         return Build_Struct_Type
           ((1 => P_Data_T,
             2 => Pointer_Type (Create_Array_Bounds_Type (GT), Address_Space)),
            Name        => Name,
            Field_Names => (0 => Data_Name, 1 => Name_Find ("P_BOUNDS")));
      end if;
   end Create_Array_Fat_Pointer_Type_Internal;

   ------------------------------
   -- Create_Array_Bounds_Type --
   ------------------------------

   function Create_Array_Bounds_Type
     (GT : Array_Or_PAT_GL_Type) return Type_T
   is
      BT      : constant Array_Or_PAT_GL_Type := Array_Base_GL_Type (GT);
      Result  : Type_T                        := Get_Bounds_Type (GT);
      To_Save : Boolean                       := True;

   begin
      --  If we already made one, nothing to do

      if Present (Result) then
         To_Save := False;

      --  Otherwise, see if this isn't the base type. If so, get the type
      --  for it

      elsif BT /= GT then
         Result := Create_Array_Bounds_Type (BT);

      --  Otherwise, build one

      else
         Result := Create_Array_Bounds_Type_Internal (GT);
      end if;

      --  Set the type if it wasn't already and then return it

      if To_Save then
         Set_Bounds_Type (GT, Result);
      end if;

      return Result;
   end Create_Array_Bounds_Type;

   ---------------------------------------
   -- Create_Array_Bounds_And_Data_Type --
   ---------------------------------------

   function Create_Array_Bounds_And_Data_Type
     (GT : Array_Or_PAT_GL_Type) return Type_T
   is
      Result  : Type_T := Get_Bounds_And_Data_Type (GT);

   begin
      --  If we haven't made one, build it and save it

      if No (Result) then
         Result := Create_Array_Bounds_And_Data_Type_Internal (GT);
         Set_Bounds_And_Data_Type (GT, Result);
      end if;

      return Result;
   end Create_Array_Bounds_And_Data_Type;

   ------------------------------
   -- Create_Array_Fat__Pointer_Type --
   ------------------------------

   function Create_Array_Fat_Pointer_Type
     (GT : Array_Or_PAT_GL_Type) return Type_T
   is
      BT      : constant Array_Or_PAT_GL_Type := Array_Base_GL_Type (GT);
      Result  : Type_T                        := Get_Fat_Pointer_Type (GT);
      To_Save : Boolean                       := True;

   begin
      --  If we already made one, nothing to do

      if Present (Result) then
         To_Save := False;

      --  Otherwise, see if this isn't the base type. If so, get the type
      --  for it

      elsif BT /= GT then
         Result := Create_Array_Fat_Pointer_Type (BT);

      --  Otherwise, build one

      else
         Result := Create_Array_Fat_Pointer_Type_Internal (GT);
      end if;

      --  Set the type if it wasn't already and then return it
      if To_Save then
         Set_Fat_Pointer_Type (GT, Result);
      end if;

      return Result;
   end Create_Array_Fat_Pointer_Type;

begin
   --  Make a dummy entry in the array info tables, so the "Empty"
   --  entry is never used.

   Array_Info.Increment_Last;
   Array_Types.Increment_Last;

end GNATLLVM.Arrays.Create;
