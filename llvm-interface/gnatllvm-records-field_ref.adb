------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2024, AdaCore                     --
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

with Table;  use Table;
with Ttypes; use Ttypes;

with GNATLLVM.Arrays;       use GNATLLVM.Arrays;
with GNATLLVM.Builtins;     use GNATLLVM.Builtins;
with GNATLLVM.Compile;      use GNATLLVM.Compile;
with GNATLLVM.Codegen;      use GNATLLVM.Codegen;
with GNATLLVM.Conversions;  use GNATLLVM.Conversions;
with GNATLLVM.Instructions; use GNATLLVM.Instructions;
with GNATLLVM.Utils;        use GNATLLVM.Utils;
with GNATLLVM.Wrapper;      use GNATLLVM.Wrapper;

package body GNATLLVM.Records.Field_Ref is

   -------------------
   -- Is_Normalized --
   -------------------

   function Is_Normalized (BRD : Bitfield_Ref_Desc) return Boolean is
   begin
      return Present (BRD) and then BRD.Size <= Max_Int_Size
        and then not Is_Padded_GL_Type (BRD.GT)
        and then (not Is_Reference (BRD.LHS) or else BRD.Offset < BPU);
   end Is_Normalized;

   ---------------
   -- Normalize --
   ---------------

   function Normalize (BRD : Bitfield_Ref_Desc) return Bitfield_Ref_Desc is
      Out_BRD : Bitfield_Ref_Desc := BRD;

   begin
      --  First, see if we have a padded GL_Type. If so, we want to reference
      --  the primitive version of the type.

      if Is_Padded_GL_Type (Out_BRD.GT) then
         Out_BRD.GT := Primitive_GL_Type (Out_BRD.GT);
         Out_BRD.Size := +GT_Size (Out_BRD.GT);
      end if;

      --  Next see if we have a reference but the bit offet is larger
      --  than a byte. If so, update the address and bit offset.

      if Is_Reference (Out_BRD.LHS) and then Out_BRD.Offset >=  BPU then
         Out_BRD.LHS := Ptr_To_Relationship (Out_BRD.LHS, Void_Ptr_T,
                                             Out_BRD.GT, Reference_To_Unknown);
         Out_BRD.LHS    := GEP_To_Relationship
           (SSI_GL_Type, Reference_To_Unknown, Out_BRD.LHS, Byte_T,
            (1 => Size_Const_Int (ULL (Out_BRD.Offset / BPU))));
         Out_BRD.Offset := Out_BRD.Offset mod BPU;
      end if;

      return Out_BRD;
   end Normalize;

   ---------------------------
   -- Collect_Mixed_Bifield --
   ---------------------------

   function Collect_Mixed_Bitfield
     (In_N       : N_Subexpr_Id;
      For_LHS    : Boolean := False;
      Prefer_LHS : Boolean := False) return Bitfield_Ref_Desc
   is
      package Refs is new Table.Table
        (Table_Component_Type => N_Subexpr_Id,
         Table_Index_Type     => Nat,
         Table_Low_Bound      => 1,
         Table_Initial        => 5,
         Table_Increment      => 2,
         Table_Name           => "Refs");

      Last_Bitfield : Nat          := 0;
      N             : N_Subexpr_Id := In_N;
      Bit_Offset    : Nat          := 0;
      Offset        : GL_Value     := Size_Const_Null;
      GT            : GL_Type;
      Result        : GL_Value;
      Size          : Nat;

   begin
      --  Skip conversions. Record all record and arrays references in our
      --  table, and mark the last bitfield reference we see, if any.
      --  ??? We should support N_Slice here, but it should be rare and is
      --  a bit of a mess.

      while Present (N) loop

         --  We can't handle this specially if Volatile Full Access
         --  is specified.

         if Is_VFA_Ref (N) then
            exit;
         end if;

         case Nkind (N) is
            when N_Unchecked_Type_Conversion | N_Type_Conversion
                 | N_Qualified_Expression =>
               N := Expression (N);

            when N_Indexed_Component =>
               Refs.Append (N);
               N := Prefix (N);

            when N_Selected_Component =>
               Refs.Append (N);

               if Present (Get_Field_Info (Selector_Field (N)))
                 and then Is_Bitfield (Selector_Field (N))

               then
                  Last_Bitfield := Refs.Last;
               end if;

               N := Prefix (N);

            when others =>
               exit;
         end case;
      end loop;

      --  If we didn't find a bitfield or if the only bitfield reference is
      --  the only reference we have, we do normal handling.

      if Last_Bitfield <= 1 then
         return No_BRD;
      end if;

      --  We start by going to the position of the bitfield in the outermost
      --  reference and initialzing our bit offset from that field. Force into
      --  memory since we're computing byte and bit offsets.

      N := Refs.Table (Last_Bitfield);
      Result := Emit (Prefix (N), For_LHS    => For_LHS,
                      Prefer_LHS => Prefer_LHS);
      Result := Record_Field_Offset (Get (Result, Any_Reference),
                                     Selector_Field (N));
      Result := Ptr_To_Ref (Result, SSI_GL_Type);
      Bit_Offset := +Field_Bit_Offset (Selector_Field (N));

      --  Now we process the rest of the component references, from outer
      --  to inner, updating the offsets for each.

      for J in reverse 1 .. Last_Bitfield - 1 loop
         N := Refs.Table (J);
         if Nkind (N) = N_Indexed_Component then
            Offset :=
              Offset + Compute_Index_Offset (Get_Indices (N, Result),
                                             Full_GL_Type (Prefix (N)),
                                             False, Result);
         else
            declare
               F      : constant Record_Field_Kind_Id := Selector_Field (N);
               Fidx   : constant Field_Info_Id        := Get_Field_Info (F);
               TE     : constant Record_Kind_Id       := Full_Scope (F);
               R_Ridx : constant Record_Info_Id       := Get_Record_Info (TE);
               FI     : constant Field_Info           :=
                 Field_Info_Table.Table (Fidx);
               F_Ridx : constant Record_Info_Id       := FI.Rec_Info_Idx;
               RI     : constant Record_Info          :=
                 Record_Info_Table.Table (F_Ridx);

            begin
               --  In this case, when we're nested inside of a bitfield,
               --  we should be a native LLVM struct. So check for that.

               pragma Assert (Present (RI.LLVM_Type));

               Offset := Offset +
                 To_Bytes (Get_Record_Size_So_Far (TE, Result,
                                                   Start_Idx => R_Ridx,
                                                   Idx       => F_Ridx)) +
                 Size_Const_Int (Get_Element_Offset (RI.LLVM_Type,
                                                     FI.Field_Ordinal));
               Bit_Offset := Bit_Offset + (+Field_Bit_Offset (F));
            end;
         end if;

      end loop;

      --  We want to always point within one byte, so if we've accumulated
      --  more than one byte of bit offset, normalize. This is redundant
      --  with the normalization call below, but we want to combine
      --  the offsets.

      if Bit_Offset >=  BPU then
         Offset := Offset + Size_Const_Int (ULL (Bit_Offset / BPU));
         Bit_Offset := Bit_Offset mod BPU;
      end if;

      --  If we have an offset to add, do it via a GEP

      if not Is_Const_0 (Offset) then
         Result := GEP (SSI_GL_Type, Result, (1 => Offset));
      end if;

      --  We get the size and type from the innermost reference

      N := Refs.Table (1);
      if Nkind (N) = N_Selected_Component then
         GT   := Field_Type (Selector_Field (N));
         Size := +Esize (Selector_Field (N));
      else
         pragma Assert (Nkind (N) = N_Indexed_Component);

         Size := +Component_Size (Full_Etype (Prefix (N)));
         GT   := Full_Component_GL_Type (Full_Etype (Prefix (N)));
      end if;

      --  Finally, return all the needed information to desscribe the access

      return Normalize ((Result, GT, Bit_Offset, Size));

   end Collect_Mixed_Bitfield;

   -------------------------
   -- Build_Bitfield_Load --
   -------------------------

   function Build_Bitfield_Load
     (BRD : Bitfield_Ref_Desc; LHS : GL_Value := No_GL_Value) return GL_Value
   is
      F_GT        : constant GL_Type := BRD.GT;
      F_Bits      : constant Nat     :=
        Nat (ULL'(+Get_Type_Size (F_GT, Max_Size => True)));
      Num_Bits    : constant Nat     := BRD.Size;
      First_Bit   : constant Nat     := BRD.Offset;
      Needed_Bits : constant Nat     := Byte_Align (First_Bit + Num_Bits);
      Our_Bits    : constant Nat     :=
        (if   Is_Reference (BRD.LHS) then Needed_Bits
         else Nat (Get_Scalar_Bit_Size (Type_Of (BRD.LHS))));
      Result      : GL_Value         := BRD.LHS;

   begin
      --  If we have constant data for a bitfield. convert it to an integer
      --  type of the same width as the input. If it's already of that
      --  type, this is a noop.

      if Relationship (Result) = Unknown and then Is_Constant (Result) then
         Result := G (Convert_Aggregate_Constant (+Result, Int_Ty (Our_Bits)),
                      F_GT, Unknown);
      end if;

      --  LLVM IR's load and store instructions are only defined when they're
      --  loading an integral number of bytes. We computed above the minimum
      --  number of bits needed to load all the bytes containg the needed
      --  bitfield. If we're not emitting C or if there's an integer type
      --  wide enough to represent that number of bytes, we can load those
      --  bytes, shift the data to the low-order bits, and truncate.

      if not Emit_C or else Our_Bits <= Max_Int_Size then
         declare
            T : constant Type_T :=
              (if   Is_Reference (Result) then Int_Ty (Needed_Bits)
               else Type_Of (Result));

         begin
            --  If we have data, we have the entire bitfield. So all we have
            --  to do is shift. If not, we have to pick up the precise
            --  location within the field.

            if Is_Reference (Result) then
               Result := Ptr_To_Relationship
                 (Result, Pointer_Type (T, Address_Space),
                  Reference_To_Unknown);

               Set_Unknown_T (Result, T);
               Result := Get (Result, Unknown);
            end if;

            --  Now shift and truncate, if necessary

            if First_Bit /= 0 then
               Result := L_Shr (Result,
                                G (Const_Int (T, ULL (First_Bit), False),
                                   F_GT, Unknown));
            end if;

            if Num_Bits /= Our_Bits then
               Result := Trunc_To_Relationship (Result, Int_Ty (Num_Bits),
                                                Unknown);
            end if;
         end;

      --  If we need more bits than we have available to us, the front end
      --  and our handling has ensured that it must be the case that we have
      --  a field of the largest bitsize that needs one additional
      --  byte. First verify this. Then, load that byte, extend it to the
      --  maximum integer type, load the field, do a right funnel shift to
      --  extract the needed bits, and truncate to the actual size, if
      --  necessary.

      elsif Needed_Bits = Max_Int_Size + BPU then
         declare
            Low_Addr    : constant GL_Value :=
              (if   Bytes_Big_Endian
               then GEP_To_Relationship (Max_Int_GL_Type, Reference_To_Unknown,
                                         Result, (1 => Size_Const_Int (1)))
               else Result);
            High_Addr   : constant GL_Value :=
              (if   Bytes_Big_Endian then Result
               else GEP (A_Char_GL_Type, Result,
                         (1 => To_Bytes (Get_Type_Size (Max_Int_T)))));
            Byte        : constant GL_Value := Load (High_Addr);
            High_Part   : constant GL_Value :=
              Z_Ext_To_Relationship (Byte, Max_Int_T, Unknown);
            Low_Ptr     : constant GL_Value :=
              Convert_Ref (Low_Addr, Max_Int_GL_Type);
            Low_Part    : constant GL_Value := Load (Low_Ptr);

         begin
            Result := Call_Intrinsic ("llvm.fshr",
                                      (1 => High_Part, 2 => Low_Part,
                                       3 => Const_Int (Max_Int_GL_Type,
                                                       ULL (First_Bit),
                                                       False)));

            if Num_Bits /= Max_Int_Size then
               Result := Trunc_To_Relationship (Result, Int_Ty (Num_Bits),
                                                Unknown);
            end if;
         end;

      --  Otherwise, we can't do this

      else
         raise Program_Error with "invalid bitfield load";
      end if;

      --  At this point, we have an integer type and now have to
      --  possibly convert it to the type of the field.  There are a
      --  few possibilies that type. If it's an integer, we
      --  may have to extend or truncate to the appropriate integer
      --  type.

      if Is_Integer_Type (F_GT) or else Is_Enumeration_Type (F_GT) then
         if Num_Bits = F_Bits then
            return G_Is_Relationship (Result, F_GT, Data);
         elsif Num_Bits > F_Bits then
            return Trunc (Result, F_GT);
         elsif Is_Unsigned_Type (F_GT) then
            return Z_Ext (Result, F_GT);
         else
            return S_Ext (Result, F_GT);
         end if;

         --  If the result is a pointer or FP type, the sizes must match
         --  and we do the conversion by casting bits.

      elsif Is_Floating_Point_Type (F_GT) then
         return Bit_Cast (Result, F_GT);
      elsif  Is_Access_Type (F_GT) then
         return Int_To_Ptr (Result, F_GT);

         --  The final case is when we have a small structure. In that case,
         --  the bitcast instruction isn't supported and we have no choice
         --  but to store the data into memory.

      else
         declare
            Memory         : constant GL_Value :=
              (if   Present (LHS) then LHS
               else Allocate_For_Type (F_GT));
            Mem_As_Int_Ptr : GL_Value          :=
              Ptr_To_Relationship
                (Memory, Pointer_Type (Type_Of (Result), Address_Space),
                 F_GT, Reference_To_Unknown);

         begin
            Set_Unknown_T (Mem_As_Int_Ptr, Type_Of (Result));
            Store (Result, Mem_As_Int_Ptr);
            return Memory;
         end;
      end if;
   end Build_Bitfield_Load;

   --------------------------
   -- Build_Bitfield_Store --
   --------------------------

   function Build_Bitfield_Store
     (RHS : GL_Value; BRD : Bitfield_Ref_Desc) return GL_Value
   is
      Have_Ref    : constant Boolean := Is_Reference (BRD.LHS);
      F_GT        : constant GL_Type := BRD.GT;
      F_Bits      : constant Nat     :=
        Nat (ULL'(+Get_Type_Size (F_GT, Max_Size => True)));
      Num_Bits    : constant Nat     := BRD.Size;
      First_Bit   : constant Nat     := BRD.Offset;
      Needed_Bits : constant Nat     := Byte_Align (First_Bit + Num_Bits);
      Our_Bits    : constant Nat     :=
        (if   Have_Ref then Needed_Bits
         else Nat (Get_Scalar_Bit_Size (Type_Of (BRD.LHS))));
      F_T         : constant Type_T  := Int_Ty (F_Bits);
      New_F_T     : constant Type_T  := Int_Ty (Num_Bits);
      New_RHS     : GL_Value         := Convert_GT (RHS, F_GT);

   begin
      --  We start by making sure that RHS is an integer. If it's a floating
      --  point or access type, we bitcast to an integer of the same width.

      if Is_Floating_Point_Type (F_GT) then
         New_RHS := Bit_Cast_To_Relationship (New_RHS, New_F_T, Unknown);
      elsif Is_Access_Type (F_GT) then
         New_RHS := Ptr_To_Int_To_Relationship (New_RHS, New_F_T, Unknown);

         --  Otherwise, unless it's an integral type, we need to either
         --  pointer-pun it into an integral type or convert it if it's a
         --  constant.

      elsif not Is_Integer_Type (F_GT) and then not Is_Enumeration_Type (F_GT)
      then
         if Is_Nonsymbolic_Constant (New_RHS) then
            New_RHS := G (Convert_Aggregate_Constant (+New_RHS, F_T),
                          F_GT, Unknown);
         else
            New_RHS := Get (New_RHS, Reference);
            New_RHS :=
              Ptr_To_Relationship (New_RHS,
                                   Pointer_Type (F_T, Address_Space),
                                   Reference_To_Unknown);
            Set_Unknown_T (New_RHS, F_T);
         end if;
      end if;

      --  Now ensure RHS is loaded and truncate the RHS to the number of
      --  bits corresponding to the field.

      if Is_Reference (New_RHS) then
         New_RHS := Load (New_RHS);
      end if;

      New_RHS := Trunc_To_Relationship (New_RHS, New_F_T, Unknown);

      --  Similarly to the load case, if we're not emitting C or if
      --  there's an integer type wide enough to represent that number of
      --  bytes, we can load those bytes, shift the RHS to the proper place,
      --  mask out those bits, and "or" in the RHS.

      if not Emit_C or else Our_Bits <= Max_Int_Size then
         declare
            Orig_T      : constant Type_T   := Type_Of (BRD.LHS);
            T           : constant Type_T   := Int_Ty (Our_Bits);
            Shift_Count : constant GL_Value :=
              G (Const_Int (T, ULL (First_Bit), False), F_GT, Unknown);
            Ones        : constant GL_Value :=
              Z_Ext_To_Relationship (G (Const_Ones (New_F_T), F_GT, Unknown),
                                     T, Unknown);
            Mask        : constant GL_Value := Shl (Ones, Shift_Count);
            Ext_RHS     : constant GL_Value :=
              Z_Ext_To_Relationship (New_RHS, T, Unknown);
            Shifted_RHS : constant GL_Value := Shl (Ext_RHS, Shift_Count);
            LHS         : GL_Value          := BRD.LHS;
            LHS_Ptr     : GL_Value;

         begin
            --  If we have a refererence, we need to load the data in our new
            --  integral type. If it's an undef, we start with zero. If it's
            --  a constant, we convert it to an integer. If it's an undefined
            --  address, do nothing.

            if Have_Ref then
               if Is_Undef (LHS) then
                  return No_GL_Value;
               end if;

               LHS_Ptr := Ptr_To_Relationship
                 (LHS, Pointer_Type (T, Address_Space), Reference_To_Unknown);
               Set_Unknown_T (LHS_Ptr, T);
               LHS := Load (LHS_Ptr);
            elsif Is_Undef (LHS) then
               LHS := G (Const_Null (T), F_GT, Unknown);
            elsif Is_Constant (LHS) then
               LHS := G (Convert_Aggregate_Constant (+LHS, T), F_GT, Unknown);
            end if;

            --  If the value we're inserting is the same as the mask (which
            --  will occur when setting a boolean True, we don't need to
            --  do the masking.

            if Mask /= Shifted_RHS then
               LHS := Build_And (LHS, Build_Not (Mask));
            end if;

            --  Now "or" in the new data

            LHS := Build_Or (LHS, Shifted_RHS);

            --  If we had a reference, store the data back. Otherwise, return
            --  the new contents of the bitfield field, possibly converted
            --  back to the original type (in the constant case).

            if Have_Ref then
               Store (LHS, LHS_Ptr);
               return No_GL_Value;
            else
               if Type_Of (LHS) /= Orig_T then
                  if Is_Undef (LHS) then
                     LHS := G (Get_Undef (Orig_T), F_GT, Unknown);
                  else
                     LHS := G (Convert_Aggregate_Constant (+LHS, Orig_T), F_GT,
                               Unknown);
                  end if;
               end if;

               return LHS;
            end if;
         end;

         --  If we need more bits than we have available to us, the front
         --  end and our handling has ensured that it must be the case that
         --  we have a field of the largest bitsize that needs one
         --  additional byte. First verify this. Then load the byte and the
         --  word, mask off the bytes corresponding to the data to store,
         --  "or" in the data, and store them back. Both the mask and data
         --  to store are shifted an amount corresponding to the location
         --  of the first bit.  We shift the word left that number of bits
         --  to get the new low-order part and right the maximum integer
         --  size minus the first bit position to get the high-order part.

      elsif Have_Ref and then Needed_Bits = Max_Int_Size + BPU then
         declare
            Low_Addr    : constant GL_Value :=
              (if   Bytes_Big_Endian
               then GEP_To_Relationship (Max_Int_GL_Type, Reference_To_Unknown,
                                         BRD.LHS, (1 => Size_Const_Int (1)))
               else Convert_Ref (BRD.LHS, SSI_GL_Type));
            High_Addr   : constant GL_Value :=
              (if   Bytes_Big_Endian then BRD.LHS
               else GEP (A_Char_GL_Type, BRD.LHS,
                         (1 => To_Bytes (Get_Type_Size (New_F_T)))));
            Low_Ptr     : constant GL_Value :=
              Convert_Ref (Low_Addr, Max_Int_GL_Type);
            Shift_Cnt_L : constant GL_Value :=
              G (Const_Int (Max_Int_T, ULL (First_Bit), False), F_GT, Unknown);
            Shift_Cnt_H : constant GL_Value :=
              G (Const_Int (Max_Int_T, ULL (Max_Int_Size - First_Bit), False),
                 F_GT, Unknown);
            Ones        : constant GL_Value :=
              Z_Ext_To_Relationship (G (Const_Ones (New_F_T), F_GT, Unknown),
                                     New_F_T, Unknown);
            Low_Mask    : constant GL_Value := Shl (Ones, Shift_Cnt_L);
            High_Mask   : constant GL_Value :=
              Trunc_To_Relationship (L_Shr (Ones, Shift_Cnt_H), Byte_T,
                                     Unknown);
            Low_Data    : constant GL_Value := Shl (New_RHS, Shift_Cnt_L);
            High_Data   : constant GL_Value :=
              Trunc_To_Relationship (L_Shr (New_RHS, Shift_Cnt_H), Byte_T,
                                     Unknown);
            Low_Part    : GL_Value          := Load (Low_Ptr);
            High_Part   : GL_Value          := Load (High_Addr);

         begin
            --  Verify that the field type is the maximum integer type
            --  since the calculations above depend on it, do the logical
            --  operations, and store the values back.

            pragma Assert (New_F_T = Max_Int_T);
            if Low_Mask /= Low_Data then
               Low_Part := Build_And (Low_Part, Build_Not (Low_Mask));
            end if;

            if High_Mask /= High_Data then
               High_Part := Build_And (High_Part, Build_Not (High_Mask));
            end if;

            Store (Build_Or (Low_Part,  Low_Data),  Low_Ptr);
            Store (Build_Or (High_Part, High_Data), High_Addr);
            return No_GL_Value;
         end;

         --  Otherwise, we can't handle this case

      else
         raise Program_Error with "invalid bitfield store";
      end if;

   end Build_Bitfield_Store;

   --------------------------
   -- Build_Bitfield_Store --
   --------------------------

   procedure Build_Bitfield_Store (RHS : GL_Value; BRD : Bitfield_Ref_Desc) is
   begin
      Discard (Build_Bitfield_Store (RHS, BRD));
   end Build_Bitfield_Store;
end GNATLLVM.Records.Field_Ref;
