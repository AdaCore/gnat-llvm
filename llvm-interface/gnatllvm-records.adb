------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2025, AdaCore                     --
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

with Elists;     use Elists;
with Nlists;     use Nlists;
with Output;     use Output;
with Repinfo;    use Repinfo;
with Sem_Ch13;   use Sem_Ch13;
with Snames;     use Snames;
with Sprint;     use Sprint;
with Uintp.LLVM; use Uintp.LLVM;

with GNATLLVM.Compile;           use GNATLLVM.Compile;
with GNATLLVM.Conditionals;      use GNATLLVM.Conditionals;
with GNATLLVM.Conversions;       use GNATLLVM.Conversions;
with GNATLLVM.DebugInfo;         use GNATLLVM.DebugInfo;
with GNATLLVM.Exprs;             use GNATLLVM.Exprs;
with GNATLLVM.Instructions;      use GNATLLVM.Instructions;
with GNATLLVM.Records.Field_Ref; use GNATLLVM.Records.Field_Ref;
with GNATLLVM.Subprograms;       use GNATLLVM.Subprograms;
with GNATLLVM.Utils;             use GNATLLVM.Utils;

package body GNATLLVM.Records is

   --  When computing the size of a record subtype, we push the subtype so
   --  we can see if we run into a discriminant from its base type. If we
   --  do, we substitute the expression that corresponds to the discriminant
   --  type. In most cases, but not all, the front end already does this
   --  substitution for us. However, we have to be sure that we don't use
   --  the same entry more than once since this could cause infinite recursion.

   type SS_Entry is record
      TE   : E_Record_Subtype_Id;
      Used : Boolean;
   end record;

   package Subtype_Stack is new Table.Table
     (Table_Component_Type => SS_Entry,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 2,
      Table_Name           => "Subtype_Stack");

   function RI_Size_Complexity
     (Idx : Record_Info_Id; Max_Size : Boolean) return Nat
     with Pre => Present (Idx);
   --  Return the size complexity of the RI designated by Idx

   function Get_Variant_Size
     (RI          : Record_Info;
      V           : GL_Value;
      In_Size     : GL_Value;
      Force_Align : Nat;
      No_Padding : Boolean := False) return GL_Value
     with Pre  => RI.Variants /= null and then RI.Overlap_Variants /= null
                  and then Present (In_Size),
          Post => Present (Get_Variant_Size'Result);
   --  Compute the total size of a record with the information from a fragment
   --  known to be a variant and where we're not getting the maximum size.

   function Get_Variant_Size
     (RI          : Record_Info;
      V           : GL_Value;
      In_Size     : IDS;
      Force_Align : Nat;
      No_Padding : Boolean := False) return IDS
     with Pre  => RI.Variants /= null and then RI.Overlap_Variants /= null
                  and then Present (In_Size),
          Post => Present (Get_Variant_Size'Result);
   --  Version of above for Is_Dynamic_Size

   function Get_Variant_Size
     (RI          : Record_Info;
      V           : GL_Value;
      In_Size     : BA_Data;
      Force_Align : Nat;
      No_Padding  : Boolean := False) return BA_Data
     with Pre => RI.Variants /= null and then RI.Overlap_Variants /= null;
   --  Version of above for back-annotation

   function Variant_Part_Size
     (RI          : Record_Info;
      V           : GL_Value;
      J           : Int;
      In_Size     : GL_Value;
      Force_Align : Nat;
      Max_Size    : Boolean := False;
      No_Padding  : Boolean := False) return GL_Value
     with Pre  => RI.Variants /= null and then RI.Overlap_Variants /= null
                  and then J in RI.Variants'Range and then Present (In_Size),
          Post => Present (Variant_Part_Size'Result);
   --  Computes the contribution to Total_Size of the variant part with
   --  index J in RI. In_Size is the total size not considering the
   --  variant and the known alignment at that point.

   function Variant_Part_Size
     (RI          : Record_Info;
      V           : GL_Value;
      J           : Int;
      In_Size     : IDS;
      Force_Align : Nat;
      Max_Size    : Boolean := False;
      No_Padding  : Boolean := False) return IDS
     with Pre  => RI.Variants /= null and then RI.Overlap_Variants /= null
                  and then J in RI.Variants'Range and then Present (In_Size),
          Post => Present (Variant_Part_Size'Result);
   --  Version of above for computing if something is dynamic size

   function Variant_Part_Size
     (RI          : Record_Info;
      V           : GL_Value;
      J           : Int;
      In_Size     : BA_Data;
      Force_Align : Nat;
      Max_Size    : Boolean := False;
      No_Padding  : Boolean := False) return BA_Data
     with Pre => RI.Variants /= null and then RI.Overlap_Variants /= null
                 and then J in RI.Variants'Range;
   --  Version of above for back-annotation

   function Align_To
     (V : IDS; Cur_Align, Must_Align : Nat) return IDS
     with Pre => Present (V), Post => Present (Align_To'Result);
   --  Version for computing whether something is dynamic size

   function Align_To
     (V : BA_Data; Cur_Align, Must_Align : Nat) return BA_Data
     with Pre => Present (V), Post => Present (Align_To'Result);
   --  Version for computing back-annotation

   --  We put the routines used to compute sizes into a generic so that we
   --  can instantiate them using various types of sizing. The most common
   --  case is an actual size computation, where we produce a GL_Value.
   --  But we may also instantiate this package to generate the structure
   --  needed for back-annotation.

   generic
      type Result is private;
      No_Result : Result;
      with function Size_Const_Int
        (C : ULL; Sign_Extend : Boolean := False) return Result;
      with function Get_Type_Size
        (GT         : GL_Type;
         V          : GL_Value := No_GL_Value;
         Max_Size   : Boolean := False;
         No_Padding : Boolean := False) return Result;
      with function Get_Variant_Size
        (RI          : Record_Info;
         V           : GL_Value;
         In_Size     : Result;
         Force_Align : Nat;
         No_Padding  : Boolean := False) return Result;
      with function "+" (V1, V2 : Result) return Result;
      with function "-" (V1, V2 : Result) return Result;
      with function Neg (V : Result; Name : String := "") return Result;
      with function Build_And
        (V1, V2 : Result; Name : String := "") return Result;
      with function Build_Max
        (V1, V2 : Result; Name : String := "") return Result;
      with function Is_A_Constant_Int       (V : Result)    return Boolean;
      with function Get_Const_Int_Value_ULL (V : Result)    return ULL;
      with function Replace_Val             (O, N : Result) return Result;
   package Size is

      function No      (V : Result) return Boolean is (V =  No_Result);
      function Present (V : Result) return Boolean is (V /= No_Result);

      function Is_Const_0 (V : Result) return Boolean is
        (Is_A_Constant_Int (V) and then Get_Const_Int_Value_ULL (V) = 0);

      function Only_Overlap_RIs (Idx : Record_Info_Id) return Boolean;
      --  Return True if Idx is null or if the only RIs after it are RIs
      --  for variants with only overlap parts.

      procedure Get_RI_Info
        (RI          : Record_Info;
         V           : GL_Value;
         Max_Size    : Boolean;
         Cur_Align   : Nat;
         Force_Align : Nat;
         Total_Size  : in out Result;
         Must_Align  : out Nat;
         Is_Align    : out Nat;
         Return_Size : Boolean := True;
         No_Padding  : Boolean := False);
      --  Return information about a record fragment RI. This includes the
      --  amount to which this fragment must be aligned and the amout to
      --  which the resulting size is known to be aligned. Also update the
      --  total size with the size of the fragment. If the size isn't
      --  wanted, don't compute it. Cur_Align is the known alignment of
      --  the size so far.

      function Get_Variant_For_RI
        (In_RI       : Record_Info;
         V           : GL_Value;
         Max_Size    : Boolean;
         Need_Idx    : Record_Info_Id;
         Use_Overlap : Boolean := False) return Record_Info_Id
        with Pre => Present (Need_Idx);
      --  We are at RI when walking the description for a record and
      --  it has variants. We're looking for Need_Idx. If Need_Idx is an
      --  index in one of the variants, return that variant.

      function Get_Variant_Max_Size
        (RI          : Record_Info;
         In_Size     : Result;
         Force_Align : Nat;
         No_Padding  : Boolean := False) return Result
        with Pre  => (RI.Variants /= null or else RI.Overlap_Variants /= null)
                     and then Present (In_Size),
             Post => Present (Get_Variant_Max_Size'Result);
      --  Get information corresponding to the maxium size of the variant
      --  described by RI, In_Size, and Force_Align.

      function Get_Record_Size_So_Far
        (TE          : Opt_Record_Kind_Id;
         V           : GL_Value;
         Start_Idx   : Record_Info_Id := Empty_Record_Info_Id;
         Idx         : Record_Info_Id := Empty_Record_Info_Id;
         In_Size     : Result         := No_Result;
         Force_Align : Nat            := BPU;
         Max_Size    : Boolean        := False;
         No_Padding  : Boolean        := False) return Result;
      --  Similar to Get_Record_Type_Size, but stop at record info segment Idx
      --  or the last segment, whichever comes first. If TE is Present, it
      --  provides the default for Start_Idx and also requests alignment to
      --  TE's alignment if we're looking for the size. Total_Size is the

      function Get_Record_Type_Size
        (TE         : Record_Kind_Id;
         V          : GL_Value;
         Max_Size   : Boolean := False;
         No_Padding : Boolean := False) return Result;
      --  Like Get_Type_Size, but only for record types

      function Emit_Field_Position
        (E : Record_Field_Kind_Id; V : GL_Value) return Result;
      --  Compute and return the position in bytes of the field specified by E
      --  from the start of its type as a value of Size_Type. If Present, V
      --  is a value of that type, which is used in the case of a
      --  discriminated record.

      function Align_To
        (V : Result; Cur_Align, Must_Align : Nat) return Result;
      --  V is a value aligned to Cur_Align. Ensure that it's aligned to
      --  Align_To.

      function Variant_Part_Size
        (RI          : Record_Info;
         V           : GL_Value;
         J           : Int;
         In_Size     : Result;
         Force_Align : Nat;
         Max_Size    : Boolean := False;
         No_Padding  : Boolean := False) return Result
        with Pre => RI.Variants /= null and then RI.Overlap_Variants /= null
             and then J in RI.Variants'Range;
   end Size;

   ------------------------
   --  RI_Value_Is_Valid --
   ------------------------

   function RI_Value_Is_Valid (RI : Record_Info_Base) return Boolean is
   begin
      --  This must be an MD_Type, which is a struct, a GL_Type, or a
      --  variant and only one of those.

      if Present (RI.MDT) then
         return No (RI.GT) and then RI.Variants = null
           and then Is_Struct (RI.MDT);
      elsif Present (RI.GT) then
         --  We already know that MDT isn't Present

         return RI.Variants = null and then RI.Overlap_Variants = null;
      else
         --  Here we know that neither type is Present

         return Present (RI.Variant_List) and then Present (RI.Variant_Expr)
           and then RI.Variants /= null;
      end if;
   end RI_Value_Is_Valid;

   ---------------------------------
   --  Use_Discriminant_For_Bound --
   ---------------------------------

   function Use_Discriminant_For_Bound (E : E_Discriminant_Id) return GL_Value
   is
      Rec_Type   : constant Record_Kind_Id := Full_Scope (E);
      TE         : constant Type_Kind_Id   := Full_Etype (E);

   begin
      --  If we're just elaborating decls, return undef

      if Decls_Only then
         return Get_Undef (Full_GL_Type (E));
      end if;

      --  See if we've pushed a subtype of this record type into our
      --  stack of record subtypes. If so, get the discriminant constraint
      --  from that subtype. But ignore a constraint on this discriminant
      --  that just repeats the discriminant.

      for J in reverse 1 .. Subtype_Stack.Last loop
         declare
            SSE : SS_Entry renames Subtype_Stack.Table (J);

         begin
            if Full_Base_Type (SSE.TE) = Rec_Type and then not SSE.Used then
               SSE.Used := True;
               return Emit_Convert_Value
                 (Get_Discriminant_Constraint (SSE.TE, E),
                  Default_GL_Type (TE));
            end if;
         end;
      end loop;

      --  Otherwise, use a value that we pushed onto the LValue stack

      return Get (Build_Field_Load (Get_Matching_Value (Rec_Type), E), Data);

   end Use_Discriminant_For_Bound;

   ---------------------------
   -- Record_Type_For_Field --
   ---------------------------

   function Record_Type_For_Field
     (GT : GL_Type; F : Record_Field_Kind_Id) return Record_Kind_Id
   is
      TE    : constant Record_Kind_Id           := Full_Etype (GT);
      New_F : constant Opt_Record_Field_Kind_Id := Find_Matching_Field (TE, F);

   begin
      --  We'd prefer to use GT's type, but only if we can find a match for
      --  F. If not, we have to use F's type.

      return (if Present (New_F) then TE else Full_Scope (F));

   end Record_Type_For_Field;

   -------------------------
   -- Find_Matching_Field --
   -------------------------

   function Find_Matching_Field
     (TE    : Record_Kind_Id;
      Field : Record_Field_Kind_Id) return Opt_Record_Field_Kind_Id
   is
      Ent : Opt_Record_Field_Kind_Id := First_Component_Or_Discriminant (TE);

   begin
      while Present (Ent) loop
         exit when Chars (Ent) = Chars (Field)
           and then Present (Get_Field_Info (Ent));
         Next_Component_Or_Discriminant (Ent);
      end loop;

      --  If this didn't work and the Original_Record_Component isn't the
      --  same record, try it and its type.

      if No (Ent) and then Original_Record_Component (Field) /= Field then
         Ent := Original_Record_Component (Field);
         return Find_Matching_Field (Full_Scope (Ent), Ent);
      end if;

      return Ent;
   end Find_Matching_Field;

   -------------------
   -- Field_Ordinal --
   -------------------

   function Field_Ordinal (F : Record_Field_Kind_Id) return unsigned is
     (unsigned (Field_Info_Table.Table (Get_Field_Info (F)).Field_Ordinal));

   ----------------
   -- Field_Type --
   ----------------

   function Field_Type (F : Record_Field_Kind_Id) return GL_Type is
      GT : constant GL_Type := Field_Info_Table.Table (Get_Field_Info (F)).GT;
   begin
      --  GT may be a dummy type. In that case, we need to get the
      --  replacement default type.

      return (if Is_Dummy_Type (GT) then Default_GL_Type (GT) else GT);
   end Field_Type;

   ------------------
   -- Parent_Field --
   ------------------

   function Parent_Field
     (F : Record_Field_Kind_Id) return Opt_Record_Field_Kind_Id
   is
      R_TE : constant Record_Kind_Id           := Full_Scope (F);
      ORC  : constant Opt_Record_Field_Kind_Id :=
        Original_Record_Component (F);
      CRC  : constant Opt_Record_Field_Kind_Id :=
        Corresponding_Record_Component (F);

   begin
      if Present (ORC) and then ORC /= F
        and then Has_Compatible_Representation (R_TE, Full_Scope (ORC))
      then
         return ORC;
      elsif Present (CRC) and then CRC /= F
        and then Has_Compatible_Representation (R_TE, Full_Scope (CRC))
      then
         return CRC;
      else
         return Empty;
      end if;

   end Parent_Field;

   --------------------
   -- Ancestor_Field --
   --------------------

   function Ancestor_Field
     (F : Record_Field_Kind_Id) return Record_Field_Kind_Id
   is
      PF : Opt_Record_Field_Kind_Id;

   begin
      return AF : Record_Field_Kind_Id := F do
         loop
            PF := Parent_Field (AF);

            if Present (PF) then
               AF := PF;
            else
               exit;
            end if;
         end loop;
      end return;
   end Ancestor_Field;

   ---------------------------------
   -- Get_Discriminant_Constraint --
   ---------------------------------

   function Get_Discriminant_Constraint
     (TE : E_Record_Subtype_Id; E : E_Discriminant_Id) return N_Subexpr_Id
   is
      Discrim_Num : constant Uint      := Discriminant_Number (E);
      Constraint  : constant Elist_Id  := Stored_Constraint (TE);
      Elmt        : Elmt_Id            := First_Elmt (Constraint);

   begin
      --  Skip to the proper entry in the list and see if it's static

      for J in Nat (1) .. +Discrim_Num - 1 loop
         Next_Elmt (Elmt);
      end loop;

      return Node (Elmt);
   end Get_Discriminant_Constraint;

   --  These are the generic functions to compute the size of record and
   --  offsets of fields within them.

   package body Size is

      ----------------------
      -- Only_Overlap_RIs --
      ----------------------

      function Only_Overlap_RIs (Idx : Record_Info_Id) return Boolean is
         J : Record_Info_Id := Idx;

      begin
         while Present (J) loop
            declare
               RI : constant Record_Info := Record_Info_Table.Table (J);

            begin
               exit when Present (RI.MDT) or else Present (RI.GT);
               exit when RI.Variants /= null
                 and then (for some K of RI.Variants.all => Present (K));

               J := RI.Next;
            end;
         end loop;

         return No (J);
      end Only_Overlap_RIs;

      -----------------
      -- Get_RI_Info --
      -----------------

      procedure Get_RI_Info
        (RI          : Record_Info;
         V           : GL_Value;
         Max_Size    : Boolean;
         Cur_Align   : Nat;
         Force_Align : Nat;
         Total_Size  : in out Result;
         Must_Align  : out Nat;
         Is_Align    : out Nat;
         Return_Size : Boolean := True;
         No_Padding  : Boolean := False)
      is
         MDT       : constant MD_Type  := RI.MDT;
         GT        : constant GL_Type  := RI.GT;
         This_Size : Result            := No_Result;

      begin
         --  If this piece has a starting position specified, move to it.

         if RI.Position /= 0 then
            Total_Size := Replace_Val (Total_Size,
                                       Size_Const_Int (RI.Position));
         end if;

         --  If we have an LLVM type, it's packed record, so our size will
         --  be that of the record and we aren't forcing an alignment. If
         --  our total size is a constant, we can say what our alignment is.

         if Present (MDT) then
            This_Size  := Size_Const_Int (Get_Type_Size (MDT));
            Must_Align := BPU;
            Is_Align   :=
              (if   Is_A_Constant_Int (Total_Size)
               then ULL_Align (Get_Const_Int_Value_ULL
                                 (Total_Size + This_Size))
               else Nat'(BPU));

         --  For a GNAT type, do similar, except that we know more about
         --  our alignment.

         elsif Present (GT) then
            Must_Align   := Get_Type_Alignment (GT);
            Is_Align     := Must_Align;

            if Return_Size then
               This_Size := Get_Type_Size (GT, V, Max_Size);
            end if;

         --  For a variant, we've already set the variant alignment, so use
         --  that for Must_Align. We can have fields after the variants
         --  in the case of extension records, so we care about Is_Align.
         --  But pessimize it rather than calculate it since the saving
         --  isn't worth it in this obscure case.

         elsif RI.Variants /= null or else RI.Overlap_Variants /= null then

            --  If we're looking for the maximum size, do our normal
            --  processing of aligning and adding the size. But if looking
            --  for the actual size, pass in the total size and let it be
            --  updated. That avoids aligning if the variant part chosen
            --  has no fields. So we return in that case.

            Must_Align := Nat'Max (Force_Align, RI.Align);
            Is_Align   := BPU;

            if Return_Size then
               Total_Size :=
                 (if   Max_Size
                  then Get_Variant_Max_Size (RI, Total_Size, Must_Align,
                                             No_Padding => No_Padding)
                  else Get_Variant_Size (RI, V, Total_Size, Must_Align,
                                         No_Padding => No_Padding));
               return;
            end if;

         --  Otherwise, this is a null entry

         else
            Must_Align := BPU;
            Is_Align   := Max_Align;
            This_Size  := Size_Const_Int (0);
         end if;

         --  Take into account any alignment we set for this RI. We
         --  needn't force alignment in the case of a forced position.

         if RI.Align /= 0 and then RI.Position = 0 then
            Must_Align := Nat'Max (Must_Align, RI.Align);
         end if;

         --  Now update the total size given what we've computed above. If
         --  this is the last RI (or subsequent RI's only have overlap RIs
         --  in variants), and we're asking for the size without padding,
         --  subtract the unused bits from the size of this piece.

         Must_Align := Nat'Max (Force_Align, Must_Align);

         if Return_Size then
            if Only_Overlap_RIs (RI.Next) and then No_Padding then
               This_Size :=
                 This_Size - Size_Const_Int (+RI.Unused_Bits);
            end if;

            Total_Size
              := Align_To (Total_Size, Cur_Align, Must_Align) + This_Size;
         end if;
      end Get_RI_Info;

      ------------------------
      -- Get_Variant_For_RI --
      ------------------------

      function Get_Variant_For_RI
        (In_RI       : Record_Info;
         V           : GL_Value;
         Max_Size    : Boolean;
         Need_Idx    : Record_Info_Id;
         Use_Overlap : Boolean := False) return Record_Info_Id
      is
         Variants : constant Record_Info_Id_Array_Access :=
           (if Use_Overlap then In_RI.Overlap_Variants else In_RI.Variants);
         New_Idx  : Record_Info_Id                       :=
           Empty_Record_Info_Id;
         Idx      : Record_Info_Id;
         RI       : Record_Info;

      begin
         --  Look through each variant

         for Variant_Idx of Variants.all loop

            --  Now look through each entry in the variant, looking into nested
            --  variants if necessary. We start looking at the first chained
            --  entry of each variant, since that's where fields of that
            --  variant start.

            Idx := Variant_Idx;
            while Present (Idx) loop
               RI := Record_Info_Table.Table (Idx);

               if Idx = Need_Idx then
                  return Variant_Idx;
               end if;

               if RI.Variants /= null then
                  New_Idx := Get_Variant_For_RI (RI, V, Max_Size,
                                                 Need_Idx, False);
                  if No (New_Idx) then
                     New_Idx := Get_Variant_For_RI (RI, V, Max_Size,
                                                    Need_Idx, True);
                  end if;

                  if Present (New_Idx) then
                     return Variant_Idx;
                  end if;
               end if;

               Idx := RI.Next;
            end loop;
         end loop;

         return Empty_Record_Info_Id;
      end Get_Variant_For_RI;

      --------------------------
      -- Get_Variant_Max_Size --
      --------------------------

      function Get_Variant_Max_Size
        (RI          : Record_Info;
         In_Size     : Result;
         Force_Align : Nat;
         No_Padding  : Boolean := False) return Result
      is
         Max_Const_Size : ULL    := 0;
         Max_Var_Size   : Result := No_Result;
         Our_Size       : Result;

      begin
         --  We need to compute the maximum size of each variant. Most
         --  discriminant sizes are constant, so we use an algorithm
         --  that'll work best in that situation. First record the largest
         --  constant size and make a chain of Max operations to compute
         --  the largest non-constant.

         for J in RI.Variants'Range loop
            Our_Size := Variant_Part_Size (RI, No_GL_Value, J, In_Size,
                                           Force_Align,
                                           Max_Size   => True,
                                           No_Padding => No_Padding);
            if Is_A_Constant_Int (Our_Size) then
               if Get_Const_Int_Value_ULL (Our_Size) > Max_Const_Size then
                  Max_Const_Size := Get_Const_Int_Value_ULL (Our_Size);
               end if;
            elsif No (Max_Var_Size) then
               Max_Var_Size := Our_Size;
            else
               Max_Var_Size := Build_Max (Our_Size, Max_Var_Size);
            end if;
         end loop;

         --  Now merge the variable and constant sizes

         if No (Max_Var_Size) then
            return Size_Const_Int (Max_Const_Size);
         elsif Max_Const_Size = 0 then
            return Max_Var_Size;
         else
            return Build_Max (Max_Var_Size, Size_Const_Int (Max_Const_Size));
         end if;
      end Get_Variant_Max_Size;

      ----------------------------
      -- Get_Record_Size_So_Far --
      ----------------------------

      function Get_Record_Size_So_Far
        (TE          : Opt_Record_Kind_Id;
         V           : GL_Value;
         Start_Idx   : Record_Info_Id := Empty_Record_Info_Id;
         Idx         : Record_Info_Id := Empty_Record_Info_Id;
         In_Size     : Result         := No_Result;
         Force_Align : Nat            := BPU;
         Max_Size    : Boolean        := False;
         No_Padding  : Boolean        := False) return Result
      is
         Total_Size   : Result         :=
           (if Present (In_Size) then In_Size else Size_Const_Int (0));
         Cur_Align    : Nat            :=
           (if   Present (In_Size) then BPU else Max_Align);
         Cur_Idx      : Record_Info_Id :=
           (if    Present (Start_Idx) then Start_Idx
            elsif Present (TE) then Get_Record_Info (TE)
            else  Empty_Record_Info_Id);
         Must_Align   : Nat            := BPU;
         Pushed_Stack : Boolean        := False;
         This_Align   : Nat;
         New_Idx      : Record_Info_Id;
         RI           : Record_Info;

      begin
         Push_Debug_Freeze_Pos;

         --  If we're passed V, add it to the list that Get_Matching_Value
         --  will search if we run into a discriminant in one of the
         --  computations below.

         if Present (V) then
            Add_To_LValue_List (V);
         end if;

         --  If this is a subtype, push it onto the stack we use to search for
         --  discriminant values.

         if Present (TE) and then Ekind (TE) = E_Record_Subtype
           and then Has_Discriminants (Full_Base_Type (TE))
           and then Is_Constrained (TE)
         then
            Subtype_Stack.Append ((TE, False));
            Pushed_Stack := True;
         end if;

         --  Look at each piece of the record and find its value and alignment.
         --  Align to the needed alignment for this piece, add its size, and
         --  show what alignment we now have.

         while Present (Cur_Idx) and then Cur_Idx /= Idx loop
            New_Idx := Empty_Record_Info_Id;
            RI      := Record_Info_Table.Table (Cur_Idx);

            --  If we're reached a variant point, we have two cases. We
            --  could be looking for a specific RI index, in which case we
            --  see which variant has that index and set it as next, or
            --  we're looking to compute the size of the record.

            if RI.Variants /= null and then Present (Idx) then
               New_Idx := Get_Variant_For_RI (RI, V, Max_Size, Idx, False);

               if Present (New_Idx) and then RI.Align /= 0 then
                  Total_Size := Align_To (Total_Size, Cur_Align, RI.Align);
               end if;

               if No (New_Idx) then
                  New_Idx := Get_Variant_For_RI (RI, V, Max_Size, Idx, True);

                  if Present (New_Idx) then
                     Total_Size := Size_Const_Int (0);
                  end if;
               end if;
            end if;

            if Present (New_Idx) then
               Cur_Idx := New_Idx;
            else
               Get_RI_Info (RI, V, Max_Size, Cur_Align, Force_Align,
                            Total_Size, Must_Align, This_Align,
                            No_Padding => No_Padding);

               --  The resulting alignment is the minimum of this alignment
               --  and the maximum of the current alignment and what we had
               --  to align to.

               Cur_Align := Nat'Min (This_Align,
                                     Nat'Max (Cur_Align, Must_Align));
               Cur_Idx   := RI.Next;
            end if;
         end loop;

         --  At this point, either Idx is not Present, meaning we were
         --  supposed to be at the end of the type, or it is, in which case
         --  we should have hit it. If either is the case, we have an
         --  error where we're looking for a field in the wrong type.

         pragma Assert (Cur_Idx = Idx);

         --  Now we may have to do a final alignment. If Idx is specified,
         --  use the alignment for that field. Otherwise, use the
         --  alignment for the type.

         if Present (Idx) then
            Get_RI_Info (Record_Info_Table.Table (Idx), No_GL_Value, False,
                         Cur_Align, BPU, Total_Size, Must_Align, This_Align,
                         Return_Size => False, No_Padding => No_Padding);
         elsif Present (TE) then
            Must_Align := Get_Type_Alignment (Default_GL_Type (TE));
         end if;

         if Pushed_Stack then
            Subtype_Stack.Decrement_Last;
         end if;

         Pop_Debug_Freeze_Pos;
         return (if   No_Padding then Total_Size
                 else Align_To (Total_Size, Cur_Align, Must_Align));
      end Get_Record_Size_So_Far;

      --------------------------
      -- Get_Record_Type_Size --
      --------------------------

      function Get_Record_Type_Size
        (TE         : Record_Kind_Id;
         V          : GL_Value;
         Max_Size   : Boolean := False;
         No_Padding : Boolean := False) return Result is

      begin
         return Get_Record_Size_So_Far
           (TE, V,
            Max_Size   => Max_Size or else Is_Unchecked_Union (TE),
            No_Padding => No_Padding);
      end Get_Record_Type_Size;

      -------------------------
      -- Emit_Field_Position --
      -------------------------

      function Emit_Field_Position
        (E : Record_Field_Kind_Id; V : GL_Value) return Result
      is
         TE     : constant Record_Kind_Id := Full_Scope (E);
         R_Idx  : constant Record_Info_Id := Get_Record_Info (TE);
         F_Idx  : constant Field_Info_Id  := Get_Field_Info (E);
         FI     : Field_Info;
         Idx    : Record_Info_Id;
         RI     : Record_Info;
         Offset : Result;

      begin
         --  If there's no field information for this field, the field
         --  position is undefined.

         if No (F_Idx) then
            return No_Result;
         end if;

         FI     := Field_Info_Table.Table (F_Idx);
         Idx    := FI.Rec_Info_Idx;
         RI     := Record_Info_Table.Table (Idx);
         Offset := Get_Record_Size_So_Far (TE, V,
                                           Start_Idx => R_Idx,
                                           Idx       => Idx);

         --  Offset now gives the offset from the start of the record to the
         --  piece that this field is in. If this piece has a GL_Type, then
         --  the field is the entire piece and we have the offset. If it's an
         --  LLVM type, we need to compute the offset within that type.

         if Present (RI.GT) then
            return Offset;
         else
            declare
               Ordinal     : constant unsigned := unsigned (FI.Field_Ordinal);
               This_Offset : constant ULL      :=
                 Offset_Of_Element (Module_Data_Layout, +RI.MDT, Ordinal);

            begin
               return Offset + Size_Const_Int (This_Offset * UBPU);
            end;
         end if;
      end Emit_Field_Position;

      --------------
      -- Align_To --
      --------------

      function Align_To
        (V : Result; Cur_Align, Must_Align : Nat) return Result is
      begin
         --  If we can determine that we needn't do any alignment, do
         --  nothing. Otherwise, align.

         if Must_Align <= Cur_Align then
            return V;
         else
            return Build_And (V + Size_Const_Int (ULL (Must_Align - 1)),
                              Neg (Size_Const_Int (ULL (Must_Align))));
         end if;
      end Align_To;

      -----------------------
      -- Variant_Part_Size --
      -----------------------

      function Variant_Part_Size
        (RI          : Record_Info;
         V           : GL_Value;
         J           : Int;
         In_Size     : Result;
         Force_Align : Nat;
         Max_Size    : Boolean := False;
         No_Padding  : Boolean := False) return Result
      is
         Variant_Size : constant Result :=
           Get_Record_Size_So_Far (Empty, V,
                                   Start_Idx   => RI.Variants (J),
                                   In_Size     => In_Size,
                                   Force_Align => Force_Align,
                                   Max_Size    => Max_Size,
                                   No_Padding  => No_Padding);
         Overlap_Size : constant Result :=
           Get_Record_Size_So_Far (Empty, V,
                                   Start_Idx => RI.Overlap_Variants (J),
                                   No_Padding => No_Padding);

      begin
         --  The resulting size is the maximum of that and the size of the
         --  overlap portion (but special-case zero).

         if Is_Const_0 (Variant_Size) then
            return Overlap_Size;
         elsif Is_Const_0 (Overlap_Size) then
            return Variant_Size;
         else
            return Build_Max (Variant_Size, Overlap_Size);
         end if;
      end Variant_Part_Size;

   end Size;

   --  Replace_Val is used to replace a size value with a new value.
   --  For both the normal and back-annotation cases, this is the second
   --  operand. However, for the purpose of determining if this is of
   --  dynamic size, we need to conside the size variable if either the old
   --  or the new value is variable to avoid trying to compute something
   --  dynamic at the global level. We could probably avoid this computation
   --  if worth it, but the case here is very unusual (involving partially-
   --  repped extension records), so it's not worth the trouble.

   function Replace_Val (Unused_O, N : GL_Value) return GL_Value is
      (N);

   function Replace_Val (Unused_O, N : BA_Data) return BA_Data is
      (N);

   function Replace_Val (O, N : IDS) return IDS is
      (if Is_Const (O) then N else Var_IDS);

   --  Here we instantiate the size routines with functions that compute
   --  the LLVM value the size and make those visible to clients. Since the
   --  addition and subtraction functions as well as binary "and" can be
   --  called with both addresses and integers, we define wrappers that
   --  either call our abstractions for address arithmetic or directly
   --  generate code for regular integer operations.

   function Address_Or_Integer_Add (V1, V2 : GL_Value) return GL_Value is
     (if Is_Address (V1) then Address_Add (V1, V2) else V1 + V2);

   function Address_Or_Integer_Sub (V1, V2 : GL_Value) return GL_Value is
     (if Is_Address (V1) then Address_Sub (V1, V2) else V1 - V2);

   function Address_Or_Integer_And
     (V1, V2 : GL_Value; Name : String := "") return GL_Value
   is
     (if Tagged_Pointers and then Is_Address (V1) then
        Set_Pointer_Address
          (V1, Build_And (Get_Pointer_Address (V1), V2, Name))
      else Build_And (V1, V2, Name));

   package LLVM_Size is
      new Size (Result                  => GL_Value,
                No_Result               => No_GL_Value,
                Size_Const_Int          => Size_Const_Int,
                "+"                     => Address_Or_Integer_Add,
                "-"                     => Address_Or_Integer_Sub,
                Neg                     => Neg,
                Build_And               => Address_Or_Integer_And,
                Build_Max               => Build_Max,
                Is_A_Constant_Int       => Is_A_Constant_Int,
                Get_Const_Int_Value_ULL => Get_Const_Int_Value_ULL,
                Get_Type_Size           => Get_Type_Size,
                Get_Variant_Size        => Get_Variant_Size,
                Replace_Val             => Replace_Val);

   function Get_Record_Size_So_Far
     (TE          : Opt_Record_Kind_Id;
      V           : GL_Value;
      Start_Idx   : Record_Info_Id := Empty_Record_Info_Id;
      Idx         : Record_Info_Id := Empty_Record_Info_Id;
      In_Size     : GL_Value       := No_GL_Value;
      Force_Align : Nat            := BPU;
      Max_Size    : Boolean        := False;
      No_Padding  : Boolean        := False) return GL_Value
     renames LLVM_Size.Get_Record_Size_So_Far;

   function Get_Record_Type_Size
     (TE         : Record_Kind_Id;
      V          : GL_Value;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return GL_Value
     renames LLVM_Size.Get_Record_Type_Size;

   function Emit_Field_Position
     (E : Record_Field_Kind_Id; V : GL_Value) return GL_Value
     renames LLVM_Size.Emit_Field_Position;

   function Align_To
     (V : GL_Value; Cur_Align, Must_Align : Nat) return GL_Value
     renames LLVM_Size.Align_To;

   function Variant_Part_Size
     (RI          : Record_Info;
      V           : GL_Value;
      J           : Int;
      In_Size     : GL_Value;
      Force_Align : Nat;
      Max_Size    : Boolean := False;
      No_Padding  : Boolean := False) return GL_Value
     renames LLVM_Size.Variant_Part_Size;

   --  Here we instantiate the size routines with functions that compute
   --  whether a size is dynamic or not and make those visible to clients.

   package IDS_Size is
      new Size (Result                  => IDS,
                No_Result               => No_IDS,
                Size_Const_Int          => Const,
                "+"                     => "+",
                "-"                     => "-",
                Neg                     => Neg,
                Build_And               => Build_And,
                Build_Max               => Build_Max,
                Is_A_Constant_Int       => Is_Const,
                Get_Const_Int_Value_ULL => Const_Val_ULL,
                Get_Type_Size           => Get_Type_Size,
                Get_Variant_Size        => Get_Variant_Size,
                Replace_Val             => Replace_Val);

   function Get_Record_Type_Size
     (TE         : Record_Kind_Id;
      V          : GL_Value;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return IDS
     renames IDS_Size.Get_Record_Type_Size;

   function Get_Record_Size_So_Far
     (TE          : Opt_Record_Kind_Id;
      V           : GL_Value;
      Start_Idx   : Record_Info_Id := Empty_Record_Info_Id;
      Idx         : Record_Info_Id := Empty_Record_Info_Id;
      In_Size     : IDS            := No_IDS;
      Force_Align : Nat            := BPU;
      Max_Size    : Boolean        := False;
      No_Padding  : Boolean        := False) return IDS
     renames IDS_Size.Get_Record_Size_So_Far;

   function Align_To (V : IDS; Cur_Align, Must_Align : Nat) return IDS
     renames IDS_Size.Align_To;

   function Variant_Part_Size
     (RI          : Record_Info;
      V           : GL_Value;
      J           : Int;
      In_Size     : IDS;
      Force_Align : Nat;
      Max_Size    : Boolean := False;
      No_Padding  : Boolean := False) return IDS
     renames IDS_Size.Variant_Part_Size;

   --  Here we instantiate the size routines with functions that compute
   --  back-annotation trees.

   package BA_Size is
      new Size (Result                  => BA_Data,
                No_Result               => No_BA,
                Size_Const_Int          => Const,
                "+"                     => "+",
                "-"                     => "-",
                Neg                     => Neg,
                Build_And               => Build_And,
                Build_Max               => Build_Max,
                Is_A_Constant_Int       => Is_Const,
                Get_Const_Int_Value_ULL => Const_Val_ULL,
                Get_Type_Size           => Get_Type_Size,
                Get_Variant_Size        => Get_Variant_Size,
                Replace_Val             => Replace_Val);

   function Get_Record_Type_Size
     (TE         : Record_Kind_Id;
      V          : GL_Value;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return BA_Data
     renames BA_Size.Get_Record_Type_Size;

   function Field_Position
     (E : Record_Field_Kind_Id; V : GL_Value) return BA_Data
     renames BA_Size.Emit_Field_Position;

   function Align_To (V : BA_Data; Cur_Align, Must_Align : Nat) return BA_Data
     renames BA_Size.Align_To;

   function Variant_Part_Size
     (RI          : Record_Info;
      V           : GL_Value;
      J           : Int;
      In_Size     : BA_Data;
      Force_Align : Nat;
      Max_Size    : Boolean := False;
      No_Padding  : Boolean := False) return BA_Data
     renames BA_Size.Variant_Part_Size;

   ----------------------
   -- Get_Variant_Size --
   ----------------------

   function Get_Variant_Size
     (RI          : Record_Info;
      V           : GL_Value;
      In_Size     : GL_Value;
      Force_Align : Nat;
      No_Padding  : Boolean := False) return GL_Value
   is
      Sizes       : GL_Value_Array (RI.Variants'Range) :=
        (others => No_GL_Value);
      Our_BB      : Basic_Block_T;
      End_BB      : Basic_Block_T;
      To_BBs      : Basic_Block_Array (RI.Variants'Range);
      From_BBs    : Basic_Block_Array (RI.Variants'Range);

   begin
      --  We first go through each variant and compute the sizes of each.
      --  We store the GL_Value's where we've computed those things along
      --  with the starting (for branching into the code) and ending (for
      --  use with Phi) basic blocks for each.
      --
      --  However, if the input size is constant, see if any variants are
      --  of a constant size and avoid creating BB's for them. We also need
      --  this logic to avoid trying to create a BB at top level for a
      --  record where each variant is the same size and hence isn't dynamic.

      if Is_A_Constant_Int (In_Size) then
         for J in RI.Variants'Range loop
            if Is_Const (IDS'(Get_Record_Size_So_Far
                                (Empty, No_GL_Value,
                                 Force_Align => Force_Align,
                                 Start_Idx   => RI.Variants (J),
                                 No_Padding  => No_Padding)))
            then
               Sizes (J) := Variant_Part_Size (RI, No_GL_Value, J, In_Size,
                                               Force_Align,
                                               No_Padding => No_Padding);
            end if;
         end loop;
      end if;

      --  If the above has computed all of the sizes and they're all
      --  equal, we know the size and needn't do anything else.

      if Present (Sizes (Sizes'First))
        and then (for all J in Sizes'Range =>
                    Sizes (J) = Sizes (Sizes'First))
      then
         return Sizes (Sizes'First);
      end if;

      --  Otherwise, get basic block info and compute any variable sizes

      Our_BB := Get_Insert_Block;
      End_BB := Create_Basic_Block;

      for J in RI.Variants'Range loop
         Disable_LV_Append := Disable_LV_Append + 1;
         To_BBs (J) := Create_Basic_Block;
         Position_Builder_At_End (To_BBs (J));

         --  If this variant is empty, the result is the total size so far.
         --  Otherwise, align the size coming in and add our size to it.

         if No (Sizes (J)) then
            Sizes (J) := Variant_Part_Size (RI, V, J, In_Size, Force_Align);
         end if;

         From_BBs (J) := Get_Insert_Block;
         Build_Br (End_BB);
         Disable_LV_Append := Disable_LV_Append - 1;
      end loop;

      --  Now emit the code to branch to the fragments we made above.
      --  However, if all the sizes are the same, we can just use that size.

      Position_Builder_At_End (Our_BB);
      Emit_Case_Code (RI.Variant_List, Emit_Expression (RI.Variant_Expr),
                      To_BBs);

      --  Now make the Phi that holds the size and return it

      Position_Builder_At_End (End_BB);
      return Build_Phi (Sizes, From_BBs);
   end Get_Variant_Size;

   ----------------------
   -- Get_Variant_Size --
   ----------------------

   function Get_Variant_Size
     (RI          : Record_Info;
      V           : GL_Value;
      In_Size     : IDS;
      Force_Align : Nat;
      No_Padding  : Boolean := False) return IDS
   is
      Size     : IDS := No_IDS;
      Our_Size : IDS;

   begin
      --  If we already have a variable size, we're done

      if not Is_Const (In_Size) then
         return Var_IDS;
      end if;

      --  Otherwise, first go through each variant and compute the size of
      --  each, looking only at constant values. If a size differs from a
      --  previous size, it's not a constant.

      for J in RI.Variants'Range loop
         Our_Size := Variant_Part_Size (RI, V, J, In_Size, Force_Align,
                                        No_Padding => No_Padding);

         --  If we haven't already set a size, set it. Otherwise, if our
         --  size differs from the size we saved, the size is variable.

         if No (Size) then
            Size := Our_Size;
         elsif Size /= Our_Size then
            Size := Var_IDS;
         end if;
      end loop;

      --  If there were variants, return their size

      return (if Present (Size) then Size else In_Size);
   end Get_Variant_Size;

   ----------------------
   -- Get_Variant_Size --
   ----------------------

   function Get_Variant_Size
     (RI          : Record_Info;
      V           : GL_Value;
      In_Size     : BA_Data;
      Force_Align : Nat;
      No_Padding  : Boolean := False) return BA_Data
   is
      function Get_Variant_Expr
        (RI : Record_Info; In_Values : BA_Data_Array) return BA_Data
        with Pre => List_Length_Non_Pragma (RI.Variant_List) =
                    In_Values'Length;
      --  Given an RI and a set of values, corresponding to the values
      --  in the order of the variants in the RI, return a BA_Data that
      --  represents an expression to compute which value is correct
      --  for a specific record object.

      Sizes : BA_Data_Array (RI.Variants'Range) := (others => No_BA);

      ----------------------
      -- Get_Variant_Expr --
      ----------------------

      function Get_Variant_Expr
        (RI : Record_Info; In_Values : BA_Data_Array) return BA_Data
      is
         Values : BA_Data_Array := In_Values;
         Expr   : BA_Data       := Values (Values'Last);

      begin
         --  We're building an expression of the form
         --
         --  (if C1 then V1 elsif C2 then V2 ... else VO)
         --
         --  where VO is the "others" alternative. We rely on the fact that,
         --  especially for alignments, that many of the values above are
         --  the same and "strike out" those values that are the same as
         --  ones we've already processed. We rely on the others alternative
         --  being last.
         --
         --  The initial expression is VO, so we start by removing any
         --  values equal to it.

         for J in Values'Range loop
            if Values (J) = Expr then
               Values (J) := No_BA;
            end if;
         end loop;

         --  Now iterate over the list of values until we've removed all of
         --  them. This is a quadratic algorithm, but even in records with
         --  large numbers of variants, the number of different sizes
         --  should be small, so that shouldn't be an issue.

         while (for some V of Values => Present (V)) loop
            declare
               Variant    : Opt_N_Variant_Id :=
                 First_Non_Pragma (RI.Variant_List);
               This_Cond  : BA_Data          := Const (0);
               This_Value : BA_Data;

            begin
               --  Search backwards for the last value

               for J in reverse Values'Range loop
                  This_Value := Values (J);
                  exit when Present (This_Value);
               end loop;

               --  Now build an OR of all the possibilities for this value.
               --  We know this can't include the "others" choice because we've
               --  removed that one above.

               for J in Values'Range loop
                  if Values (J) = This_Value then
                     pragma Assert (Present_Expr (Variant) /= Uint_1);
                     Values (J) := No_BA;
                     This_Cond  :=
                       Truth_Or (This_Cond,
                                 SO_Ref_To_BA (Present_Expr (Variant)));
                  end if;

                  Next_Non_Pragma (Variant);
               end loop;

               --  Finally, make the conditional expression

               Expr := Build_Select (This_Cond, This_Value, Expr);
            end;
         end loop;

         return Expr;
      end Get_Variant_Expr;

   begin
      --  We first go through each variant and compute the size of each

      for J in RI.Variants'Range loop
         Sizes (J) := Variant_Part_Size (RI, V, J, In_Size, Force_Align,
                                         No_Padding => No_Padding);
      end loop;

      --  Now compute the resulting size

      return Get_Variant_Expr (RI, Sizes);
   end Get_Variant_Size;

   -------------------------------
   -- Effective_Field_Alignment --
   -------------------------------

   function Effective_Field_Alignment (F : Record_Field_Kind_Id) return Nat is
      AF      : constant Record_Field_Kind_Id := Original_Record_Component (F);
      GT      : constant GL_Type              := Full_GL_Type (AF);
      F_Align : constant Nat                  := Get_Type_Alignment (GT);
      Pos     : constant Uint                 := Component_Bit_Offset (AF);
      Size    : constant Uint                 := Esize (AF);
      TE      : constant Record_Kind_Id       := Full_Scope (AF);
      R_Align : constant Nat                  :=
        (if    Known_Alignment (TE) then +Alignment (TE) * BPU
         elsif Known_RM_Size (TE) and then Strict_Alignment (TE)
         then  ULL_Align (+RM_Size (TE))
         elsif Known_Esize (TE) then ULL_Align (+Esize (TE))
         else  Nat'Max (Max_Align, F_Align));
      --  If an alignment is specified for the record, use it. If not, but
      --  a size is specified for the record and we require strict
      --  alignment, derive the alignment from that. Similarly if an
      --  Object_Size clause has been specified. Otherwise, the only
      --  contributions to alignment are the alignment of the fields.

      E_Align : constant Nat                  := Nat'Min (F_Align, R_Align);

   begin
      --  If the field can't be misaligned, its alignment always contributes
      --  directly to the alignment of the record.

      if Cant_Misalign_Field (AF, GT) then
         return F_Align;

      --  Otherwise, if the field is packable its alignment doesn't
      --  contribute to the alignment.

      elsif Field_Pack_Kind (AF, Force => True, Ignore_Size => True) /= None
      then
         return BPU;

      --  If there's no component clause use this field's alignment. But
      --  we can't use an alignment smaller than that of the record

      elsif No (Component_Clause (AF)) then
         return Nat'Min (F_Align, R_Align);

      --  Otherwise, if the alignment is consistent with the size and
      --  position of the component, use it.

      else
         return (if   Pos mod E_Align = 0 and then Size mod E_Align = 0
                 then E_Align else BPU);
      end if;

   end Effective_Field_Alignment;

   -----------------------------------
   -- Record_Has_Aliased_Components --
   -----------------------------------

   function Record_Has_Aliased_Components
     (TE : Record_Kind_Id) return Boolean
   is
      F : Opt_Record_Field_Kind_Id := First_Component_Or_Discriminant (TE);

   begin
      --  We ignore the tag since no user code can take its address

      while Present (F) loop
         exit when Is_Aliased (F) and then Chars (F) /= Name_uTag;
         Next_Component_Or_Discriminant (F);
      end loop;

      return Present (F);
   end Record_Has_Aliased_Components;

   -------------------------------
   -- Get_Record_Type_Alignment --
   -------------------------------

   function Get_Record_Type_Alignment (TE : Record_Kind_Id) return Nat is
      Field : Opt_Record_Field_Kind_Id;

   begin
      --  Use the largest effective alignment of any field

      return Largest_Align : Nat := BPU do

         --  If we're just elaborating types and this is a tagged record,
         --  we have to allow for the tag field because the front end
         --  won't create one in this mode.

         if Decls_Only and then Is_Tagged_Type (TE) then
            Largest_Align := Get_Type_Alignment (Void_Ptr_T);
         end if;

         --  Now go through each field looking for the highest effective
         --  alignment.

         Field := First_Component_Or_Discriminant (TE);
         while Present (Field) loop
            Largest_Align := Nat'Max (Largest_Align,
                                      Effective_Field_Alignment (Field));

            Next_Component_Or_Discriminant (Field);
         end loop;
      end return;
   end Get_Record_Type_Alignment;

   ---------------
   -- TBAA_Type --
   ---------------

   function TBAA_Type (Fidx : Field_Info_Id) return Metadata_T is
     (Field_Info_Table.Table (Fidx).TBAA_Type);

   -------------------
   -- Set_TBAA_Type --
   -------------------

   procedure Set_TBAA_Type (Fidx : Field_Info_Id; M : Metadata_T) is
   begin
      Field_Info_Table.Table (Fidx).TBAA_Type := M;
   end Set_TBAA_Type;

   ---------------------
   -- Field_Pack_Kind --
   ---------------------

   function Field_Pack_Kind
     (F           : Record_Field_Kind_Id;
      Force       : Boolean := False;
      Ignore_Size : Boolean := False) return Pack_Kind
   is
      AF  : constant Record_Field_Kind_Id := Ancestor_Field (F);
      GT  : constant GL_Type              := Full_GL_Type (AF);
      TE  : constant Record_Kind_Id       := Full_Scope (AF);

   begin
      --  We need to be sure that the type of the field is elaborated

      Discard (Type_Of (GT));

      --  If we have a rep clause, we'll use that rather than packing it.
      --  If the record isn't packed, neither is the field. Aliased fields
      --  or fields whose types are strictly aligned aren't packed either.
      --  If the type's size is variable or if the type is nonnative, we
      --  can't pack, even to a byte boundary.

      if Present (Component_Clause (AF))
        or else (Component_Alignment (TE) /= Calign_Storage_Unit
                   and then not Is_Packed (TE))
        or else Cant_Misalign_Field (AF, GT)
        or else Is_Nonnative_Type (GT)
        or else not Is_Static_SO_Ref (Esize (GT))
      then
         return None;

      --  We can pack to a bit boundary if the record is packed, the
      --  field's RM Size is known and smaller than the Esize (unless we're
      --  forcing packing), and the field isn't too large (unless we're to
      --  ignore that test). Otherwise, we pack only to a byte boundary.
      --  We rely here on back-annotation of types so we're sure that both
      --  sizes are set.

      else
         return (if   Is_Packed (TE) and then Is_Static_SO_Ref (RM_Size (GT))
                      and then (Ignore_Size or else RM_Size (GT) <= 64)
                      and then (Force or else RM_Size (GT) < Esize (GT))
                 then Bit else Byte);
      end if;

   end Field_Pack_Kind;

   ------------------------
   -- Is_Bitfield_By_Rep --
   ------------------------

   function Is_Bitfield_By_Rep
     (F            : Record_Field_Kind_Id;
      Pos          : Uint    := No_Uint;
      Size         : Uint    := No_Uint;
      Use_Pos_Size : Boolean := False) return Boolean
   is
      TE       : constant Type_Kind_Id := Full_Etype (F);
      Our_Pos  : constant Uint         :=
        (if    Use_Pos_Size then Pos
         elsif Known_Static_Component_Bit_Offset (F)
         then  Component_Bit_Offset (F) else No_Uint);
      Our_Size : constant Uint         :=
        (if    Use_Pos_Size then Size
         elsif Known_Static_Esize (F)    then Esize (F)
         elsif Field_Pack_Kind (F) = Bit then RM_Size (TE) else No_Uint);

   begin
      --  If the position is specified and isn't byte-aligned, it's a bitfield

      if Present (Our_Pos) and then Our_Pos mod BPU /= 0 then
         return True;

      --  If we have no specification of size, either explicitly or
      --  implicitly by packing, this isn't a bitfield

      elsif No (Our_Size) then
         return False;

      --  For integral types, we can only have sizes that are a power of
      --  two due to the way that LLVM handles types like i24.

      elsif Is_Discrete_Or_Fixed_Point_Type (TE) then
         return Our_Size not in
           Uint_8 | Uint_16 | Uint_32 | Uint_64 | Uint_128;
      else
         return Our_Size mod BPU /= 0;
      end if;

   end Is_Bitfield_By_Rep;

   -----------------
   -- Is_Bitfield --
   -----------------

   function Is_Bitfield (F : Record_Field_Kind_Id) return Boolean is
     (Present (Field_Info_Table.Table (Get_Field_Info (F)).First_Bit));

   -----------------------
   -- Is_Array_Bitfield --
   -----------------------

   function Is_Array_Bitfield (F : Record_Field_Kind_Id) return Boolean is
     (Field_Info_Table.Table (Get_Field_Info (F)).Array_Bitfield);

   -----------------------------
   -- Is_Large_Array_Bitfield --
   -----------------------------

   function Is_Large_Array_Bitfield
     (F : Record_Field_Kind_Id) return Boolean
   is
     (Field_Info_Table.Table (Get_Field_Info (F)).Large_Array_Bitfield);

   ----------------------
   -- Field_Bit_Offset --
   ----------------------

   function Field_Bit_Offset (F : Record_Field_Kind_Id) return Uint is
     (if   not Is_Bitfield (F) then Uint_0
      else Field_Info_Table.Table (Get_Field_Info (F)).First_Bit);

   ------------------------
   -- RI_Size_Complexity --
   ------------------------

   function RI_Size_Complexity
     (Idx : Record_Info_Id; Max_Size : Boolean) return Nat
   is
      RI : constant Record_Info := Record_Info_Table.Table (Idx);

   begin
      --  If this represents a GL_Type, use the complexity of that size

      if Present (RI.GT) then
         return Get_Type_Size_Complexity (RI.GT, Max_Size);

      --  If we have a variant expression, we start with the complexity
      --  of evaluating the variant (or zero if we're looking for the
      --  max size) and then add the complexity of each variant (we add
      --  because even if looking for the max size, we have to consider each
      --  variant).

      elsif Present (RI.Variant_Expr) then
         return Complexity : Nat := (if Max_Size then 0 else 2) do
            for Vidx of RI.Variants.all loop
               if Present (Vidx) then
                  Complexity :=
                    Complexity + RI_Size_Complexity (Vidx, Max_Size);
               end if;
            end loop;
         end return;

      --  Otherwise this is fixed size

      else
         return 0;
      end if;
   end RI_Size_Complexity;

   --------------------------------
   -- Get_Record_Size_Complexity --
   --------------------------------

   function Get_Record_Size_Complexity
     (TE : Record_Kind_Id; Max_Size : Boolean := False) return Nat
   is
      Cur_Idx : Record_Info_Id := Get_Record_Info (TE);

   begin
      return Complexity : Nat := 0 do
         while Present (Cur_Idx) loop
            Complexity :=
              Complexity + RI_Size_Complexity (Cur_Idx, Max_Size);
            Cur_Idx := Record_Info_Table.Table (Cur_Idx).Next;
         end loop;
      end return;
   end Get_Record_Size_Complexity;

   pragma Annotate (Xcov, Exempt_On, "Debug helpers");

   procedure Print_RI_Briefly (Ridx : Record_Info_Id);

   ----------------------
   -- Print_RI_Briefly --
   ----------------------

   procedure Print_RI_Briefly (Ridx : Record_Info_Id) is
      RI : constant Record_Info := Record_Info_Table.Table (Ridx);

   begin
      if Present (RI.GT) then
         Dump_GL_Type (RI.GT);
      elsif Present (RI.MDT) then
         Dump_MD_Type (RI.MDT);
      end if;

   end Print_RI_Briefly;

   ----------------------
   -- Print_Field_Info --
   ----------------------

   procedure Print_Field_Info (E : Record_Field_Kind_Id) is
      F_Idx : constant Field_Info_Id  := Get_Field_Info (E);
      FI    : Field_Info;

   begin
      Push_Output;
      Set_Standard_Error;
      Write_Str ("Field ");
      Write_Int (Nat (E));
      Write_Str (": ");
      pg (Union_Id (E));
      Write_Str ("Scope = ");
      Write_Int (Nat (Full_Scope (E)));
      Write_Eol;

      if Present (F_Idx) then
         FI := Field_Info_Table.Table (F_Idx);
         Write_Str ("RI => ");
         Write_Int (Nat (FI.Rec_Info_Idx));
         Write_Str (", Ordinal = ");
         Write_Int (FI.Field_Ordinal);

         if Present (FI.First_Bit) then
            Write_Str (", Bits = ");
            Write_Int (+FI.First_Bit);
            Write_Str (" .. ");
            Write_Int (+(FI.First_Bit + FI.Num_Bits) - 1);
         end if;

         if FI.Array_Bitfield then
            Write_Str (", Array Bitfield");
         end if;

         Write_Str (": ");
         Dump_GL_Type (FI.GT);
         Write_Eol;
         Print_RI_Briefly (FI.Rec_Info_Idx);
      end if;

      Pop_Output;
   end Print_Field_Info;

   -----------------------
   -- Print_Record_Info --
   -----------------------

   procedure Print_Record_Info (TE : Record_Kind_Id; Eol : Boolean := False) is

   begin
      declare
         procedure Print_One_RI
           (Ridx : Record_Info_Id; Prefix : String := "");
         procedure Print_RI_Chain
           (Start : Record_Info_Id; Prefix : String := "");

         ------------------
         -- Print_One_RI --
         ------------------
         procedure Print_One_RI
           (Ridx : Record_Info_Id; Prefix : String := "")
         is
            RI                 : constant Record_Info :=
              Record_Info_Table.Table (Ridx);
            F_Idx              : Field_Info_Id;
            FI                 : Field_Info;
            Var_Node, Choice   : Node_Id;

         begin
            Write_Str (Prefix);
            Write_Str ("RI ");
            Write_Int (Nat (Ridx));
            if Present (RI.Next) then
               Write_Str (" => ");
               Write_Int (Nat (RI.Next));
            end if;

            if RI.Align /= 0 then
               Write_Str (" align ");
               Write_Int (RI.Align);
            end if;

            if RI.Position /= 0 then
               Write_Str (" position ");
               Write_Int (Nat (RI.Position));
            end if;

            if RI.Unused_Bits /= 0 then
               Write_Str (" unused bits ");
               Write_Int (+RI.Unused_Bits);
            end if;

            Write_Eol;

            if Present (RI.GT) then
               Write_Str (Prefix);
               Dump_GL_Type (RI.GT);
            elsif Present (RI.MDT) then
               Write_Str (Prefix);
               Dump_MD_Type (RI.MDT);
            end if;

            F_Idx := RI.First_Field;
            while Present (F_Idx) loop
               FI := Field_Info_Table.Table (F_Idx);
               Write_Str (Prefix);
               Write_Str ("    Field");

               if Present (RI.MDT) then
                  Write_Str ("@");
                  Write_Int (FI.Field_Ordinal);

                  if Present (FI.First_Bit) then
                     Write_Str ("[");
                     Write_Int (+FI.First_Bit);
                     Write_Str (" .. ");
                     Write_Int (+(FI.First_Bit + FI.Num_Bits) - 1);
                     Write_Str ("]");
                  end if;
               end if;

               Write_Str (" ");
               Write_Int (Nat (FI.Field));
               Write_Str (": ");
               Sprint_Node (FI.Field);
               Write_Eol;
               F_Idx := FI.Next;
            end loop;

            if RI.Variants /= null then
               Write_Str   (Prefix);
               Write_Str   ("Variants for ");
               Sprint_Node (RI.Variant_Expr);
               Write_Eol;

               Var_Node := First (RI.Variant_List);
               for J in RI.Variants'Range loop
                  Write_Str (Prefix & "    ");
                  Write_Str ("when ");
                  Choice := First (Discrete_Choices (Var_Node));
                  loop
                     Sprint_Node (Choice);
                     Next (Choice);
                     exit when No (Choice);
                     Write_Str (" | ");
                  end loop;

                  Write_Line (" =>");

                  if Present (RI.Variants (J)) then
                     Print_RI_Chain (RI.Variants (J), Prefix & "    ");
                  end if;

                  if Present (RI.Overlap_Variants (J)) then
                     Print_One_RI (RI.Overlap_Variants (J),
                                   Prefix & "   overlap ");
                  end if;

                  Next (Var_Node);
               end loop;
            end if;
         end Print_One_RI;

         --------------------
         -- Print_RI_Chain --
         --------------------

         procedure Print_RI_Chain
           (Start : Record_Info_Id; Prefix : String := "")
         is
            Idx : Record_Info_Id := Start;

         begin
            while Present (Idx) loop
               Print_One_RI (Idx, Prefix);
               Idx := Record_Info_Table.Table (Idx).Next;
            end loop;
         end Print_RI_Chain;

      begin
         Push_Output;
         Set_Standard_Error;
         Print_RI_Chain (Get_Record_Info (TE));

         if Eol then
            Write_Eol;
         end if;

         Pop_Output;
      end;

   end Print_Record_Info;

end GNATLLVM.Records;
