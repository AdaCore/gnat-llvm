------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2020, AdaCore                     --
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
with Sinfo;    use Sinfo;
with Snames;   use Snames;

with LLVM.Core; use LLVM.Core;

with GNATLLVM.Compile;      use GNATLLVM.Compile;
with GNATLLVM.Instructions; use GNATLLVM.Instructions;
with GNATLLVM.Types.Create; use GNATLLVM.Types.Create;
with GNATLLVM.Utils;        use GNATLLVM.Utils;
with GNATLLVM.Variables;    use GNATLLVM.Variables;

package body GNATLLVM.Arrays.Create is

   function Cannot_Be_Superflat (N : Node_Id) return Boolean;
   --  Return True if the range described by N is known not to be
   --  able to be superflat.

   function Build_One_Bound
     (N             : Node_Id;
      Unconstrained : Boolean;
      For_Orig      : Boolean) return One_Bound
     with Pre => Present (N);
   --  Helper function to build a One_Bound object from N

   function Create_String_Literal_Type
     (TE : Entity_Id; Comp_Typ : Type_T) return Type_T
     with Pre  => Ekind (TE) = E_String_Literal_Subtype
                  and then Present (Comp_Typ),
          Post => (Get_Type_Kind (Create_String_Literal_Type'Result) =
                     Array_Type_Kind);
   --  Helper function to create type for string literals

   -------------------------
   -- Cannot_Be_Superflat --
   -------------------------

   function Cannot_Be_Superflat (N : Node_Id) return Boolean is
      LB      : Node_Id := Low_Bound  (N);
      HB      : Node_Id := High_Bound (N);
      TE      : Entity_Id;
      Rng     : Node_Id;

   begin
      --  If the low bound is not constant, try to find an upper bound

      loop
         exit when Nkind (LB) = N_Integer_Literal;
         TE := Full_Etype (LB);

         exit when Ekind (TE)
           not in E_Signed_Integer_Subtype | E_Modular_Integer_Subtype;

         Rng := Scalar_Range (TE);

         exit when Nkind (Rng)
           not in N_Signed_Integer_Type_Definition | N_Range;

         LB := High_Bound (Rng);
      end loop;

      --  Similarly for the high bound

      loop
         exit when Nkind (HB) = N_Integer_Literal;
         TE := Full_Etype (HB);

         exit when Ekind (TE)
           not in E_Signed_Integer_Subtype | E_Modular_Integer_Subtype;

         Rng := Scalar_Range (TE);

         exit when
           Nkind (Rng) not in N_Signed_Integer_Type_Definition | N_Range;

         HB := Low_Bound (Rng);
      end loop;

      --  If both are integers and the bounds are safe, we can't be superflat

      return Nkind (LB) = N_Integer_Literal
        and then Nkind (HB) = N_Integer_Literal
        and then Intval (HB) >= Intval (LB) - 1;
   end Cannot_Be_Superflat;

   ---------------------
   -- Build_One_Bound --
   ---------------------

   function Build_One_Bound
     (N             : Node_Id;
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
      --  value) and an integer that's in range of an Int.  If all that is
      --  true, make a Uint out of it and use it as a constant bound.

      elsif Is_No_Elab_Needed (N) then
         declare
            V   : constant GL_Value := Emit_Expression (N);
            Val : LLI;

         begin
            if Is_A_Const_Int (V) then
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
     (TE : Entity_Id; Comp_Typ : Type_T) return Type_T
   is
      First      : constant Uint         :=
        Get_Uint_Value (String_Literal_Low_Bound (TE));
      Length     : constant Uint         := String_Literal_Length (TE);
      Last       : constant Uint         := First + Length - 1;
      Low_Bound  : constant One_Bound    := (Cnst => First, Value => Empty);
      High_Bound : constant One_Bound    := (Cnst => Last, Value => Empty);
      Dim_Info   : constant Index_Bounds :=
        (Bound_GT      => Integer_GL_Type,
         Bound_Sub_GT  => Integer_GL_Type,
         Low           => Low_Bound,
         High          => High_Bound,
         Bound_Range   => Size_Const_Int (Length),
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
     (TE : Entity_Id; For_Orig : Boolean := False) return Type_T
   is
      type Dim_Info_Array is array (Nat range <>) of Index_Bounds;

      A_TE              : constant Entity_Id :=
        (if For_Orig then Full_Original_Array_Type (TE) else TE);
      Unconstrained     : constant Boolean   := not Is_Constrained (A_TE);
      CT                : constant Entity_Id := Full_Component_Type (A_TE);
      Comp_Def_GT       : constant GL_Type   := Default_GL_Type (CT);
      Comp_Size_To_Use  : constant Uint      :=
        (if    Known_Static_Component_Size (A_TE) and then not For_Orig
         then  Component_Size (A_TE)
         elsif Is_Packed (A_TE) and then not Is_Packed_Array_Impl_Type (TE)
               and then not Strict_Alignment (Comp_Def_GT)
         then  RM_Size (Comp_Def_GT) else No_Uint);
      Comp_Size         : constant Uint      :=
         Validate_Size (A_TE, Comp_Def_GT, Comp_Size_To_Use,
                        For_Component => True,
                        Zero_Allowed  => Has_Component_Size_Clause (A_TE));
      Max_Size          : constant Boolean   :=
        Is_Unconstrained_Record (Comp_Def_GT);
      Biased            : constant Boolean   :=
        Has_Biased_Representation (A_TE);
      Comp_Initial_GT   : constant GL_Type   :=
        Make_GT_Alternative (Comp_Def_GT, TE,
                             Size          => Comp_Size,
                             For_Component => True,
                             Max_Size      => Max_Size,
                             Is_Biased     => Biased);
      Comp_GT           : constant GL_Type   :=
        (if  Has_Aliased_Components (A_TE)
             and then Present (GT_Size (Comp_Initial_GT))
             and then Is_Const_Int_Value (GT_Size (Comp_Initial_GT), 0)
         then Make_GT_Alternative (Comp_Initial_GT, TE,
                                   Size          => +BPU,
                                   For_Component => True)
         else Comp_Initial_GT);
      Base_Type         : constant Entity_Id :=
        Full_Base_Type (A_TE, For_Orig);
      Must_Use_Fake     : Boolean            :=
          not Is_Native_Component_GT (Comp_GT);
      This_Nonnative    : Boolean            := Must_Use_Fake or Unconstrained;
      CT_To_Use         : constant GL_Type   :=
        (if Must_Use_Fake then SSI_GL_Type else Comp_GT);
      Typ               : Type_T             := Type_Of (CT_To_Use);
      Dim               : Nat                := 0;
      Last_Dim          : constant Nat       :=
        (if   Ekind (A_TE) = E_String_Literal_Subtype
         then 1 else Number_Dimensions (A_TE) - 1);
      Total_Size        : GL_Value           :=
        (if This_Nonnative then Size_Const_Null else Get_Type_Size (Comp_GT));
      Dim_Infos         : Dim_Info_Array (0 .. Last_Dim);
      First_Info        : Array_Info_Id;
      Index             : Entity_Id;
      Base_Index        : Entity_Id;

   begin
      --  String literal subtypes are simple, so handle then here

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

      if  Has_Atomic_Components (A_TE) or else Is_Atomic_Or_VFA (Comp_GT) then
         Check_OK_For_Atomic_Type (Comp_GT, A_TE, True);
      end if;

      --  If this is a base type, back-annotate the component size

      if Is_Base_Type (A_TE) and then Unknown_Component_Size (A_TE) then
         Set_Component_Size (A_TE, Annotated_Object_Size (Comp_GT));
      end if;

      --  We loop through each dimension of the array creating the entries
      --  for Array_Info.  If the component type is of variable size or if
      --  either bound of an index is a dynamic size, this type is of
      --  dynamic size.  We could use an opaque type in that case, but
      --  we have numerous array subtypes that should be treated identically
      --  but couldn't if we took that approach.  However, all of those
      --  subtypes will have the same component type.  If that component
      --  type is of fixed size, we can make an LLVM array [0 x CT] where
      --  CT is the component type.  Otherwise, we have to use [0 x i8].
      --  We refer to both of these cases as creating a "fake" type.

      Index      := First_Index (A_TE);
      Base_Index := First_Index (Base_Type);
      while Present (Index) loop
         declare
            Idx_Range : constant Node_Id := Get_Dim_Range (Index);
            --  Sometimes, the frontend leaves an identifier that
            --  references an integer subtype instead of a range.

            Index_GT  : constant GL_Type := Full_GL_Type (Index);
            Index_BT  : constant GL_Type := Base_GL_Type (Index_GT);
            LB        : constant Node_Id := Low_Bound (Idx_Range);
            HB        : constant Node_Id := High_Bound (Idx_Range);
            Dim_Info  : Index_Bounds     :=
              (Bound_GT      => Index_BT,
               Bound_Sub_GT  => Full_GL_Type (Base_Index),
               Low           => Build_One_Bound (LB, Unconstrained, For_Orig),
               High          => Build_One_Bound (HB, Unconstrained, For_Orig),
               Bound_Range   => No_GL_Value,
               Not_Superflat =>
                 (Ekind (TE) = E_Array_Subtype and then Nkind (Index) = N_Range
                    and then Cannot_Be_Superflat (Idx_Range))
                 or else (not For_Orig and then Is_Packed_Array_Impl_Type (TE)
                            and then Is_Bit_Packed_Array
                                       (Original_Array_Type (TE))));

            --  We have to be careful here and flag the type of the index
            --  from that of the base type since we can have index ranges
            --  that are outside the base type if the subtype is superflat
            --  (see C37172C).  We also need to record the subtype of the
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
            Next_Index (Index);
            Next_Index (Base_Index);
            Dim := Dim + 1;
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
      --  the original array type or as the primary info.  In the latter case,
      --  we do a redundant-looking setting of the type to simplify handling
      --  of the other sets.

      if For_Orig then
         Set_Orig_Array_Info   (TE, First_Info);
      else
         Set_Is_Nonnative_Type (TE, This_Nonnative);
         Set_Array_Info        (TE, First_Info);
      end if;

      return Typ;
   end Create_Array_Type;

   ------------------------------
   -- Create_Array_Bounds_Type --
   ------------------------------

   function Create_Array_Bounds_Type (TE : Entity_Id) return Type_T is
      Dims       : constant Nat           :=
        Number_Dimensions (if   Is_Packed_Array_Impl_Type (TE)
                           then Full_Original_Array_Type (TE) else TE);
      Fields     : aliased Type_Array (Nat range 0 .. 2 * Dims - 1);
      First_Info : constant Array_Info_Id :=
        (if   Is_Packed_Array_Impl_Type (TE) then Get_Orig_Array_Info (TE)
         else Get_Array_Info (TE));
      J          : Nat                    := 0;

   begin
      for K in Nat range 0 .. Dims - 1 loop
         Fields (J) := Type_Of (Array_Info.Table (First_Info + K).Bound_GT);
         Fields (J + 1) := Fields (J);
         J := J + 2;
      end loop;

      return Build_Struct_Type (Fields);
   end Create_Array_Bounds_Type;

   ---------------------------------------
   -- Create_Array_Bounds_And_Data_Type --
   ---------------------------------------

   function Create_Array_Bounds_And_Data_Type
     (TE : Entity_Id; T : Type_T) return Type_T
   is
      Align  : constant Nat    := Get_Type_Alignment (Default_GL_Type (TE));
      B_T    : constant Type_T := Create_Array_Bounds_Type (TE);
      B_T_Sz : constant Nat    := Nat (ULL'(Get_Type_Size (B_T)));

   begin
      --  If the size of the bounds type is a multiple of the alignment,
      --  we have the normal case of two types.

      if B_T_Sz mod Align = 0 then
         return Build_Struct_Type ((1 => B_T, 2 => T));

      --  Otherwise, generate some padding

      else
         declare
            Align_Sz : constant Nat := (B_T_Sz + Align - 1) / Align * Align;
            Pad      : constant Nat := (Align_Sz - B_T_Sz) / BPU;

         begin
            return
              Build_Struct_Type ((1 => B_T,
                                  2 => Array_Type (Byte_T, unsigned (Pad)),
                                  3 => T));
         end;
      end if;
   end Create_Array_Bounds_And_Data_Type;

   -----------------------------------
   -- Create_Array_Fat_Pointer_Type --
   -----------------------------------

   function Create_Array_Fat_Pointer_Type (GT : GL_Type) return Type_T is
     (Build_Struct_Type
        ((1 => Pointer_Type (Type_Of (GT), 0),
          2 => Pointer_Type (Create_Array_Bounds_Type (Full_Etype (GT)), 0))));

   -----------------------------------
   -- Create_Array_Fat_Pointer_Type --
   -----------------------------------

   function Create_Array_Fat_Pointer_Type (TE : Entity_Id) return Type_T is
     (Create_Array_Fat_Pointer_Type (Primitive_GL_Type (TE)));

begin
   --  Make a dummy entry in the array info table, so the "Empty"
   --  entry is never used.

   Array_Info.Increment_Last;

end GNATLLVM.Arrays.Create;
