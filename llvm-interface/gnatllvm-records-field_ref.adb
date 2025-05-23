------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                  Copyright (C) 2024-2025, AdaCore                   --
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

with Nlists;   use Nlists;
with Snames;   use Snames;
with Table;    use Table;
with Ttypes;   use Ttypes;

with GNATLLVM.Aliasing;     use GNATLLVM.Aliasing;
with GNATLLVM.Arrays;       use GNATLLVM.Arrays;
with GNATLLVM.Builtins;     use GNATLLVM.Builtins;
with GNATLLVM.Compile;      use GNATLLVM.Compile;
with GNATLLVM.Codegen;      use GNATLLVM.Codegen;
with GNATLLVM.Conversions;  use GNATLLVM.Conversions;
with GNATLLVM.Exprs;        use GNATLLVM.Exprs;
with GNATLLVM.Instructions; use GNATLLVM.Instructions;
with GNATLLVM.Utils;        use GNATLLVM.Utils;
with GNATLLVM.Variables;    use GNATLLVM.Variables;
with GNATLLVM.Wrapper;      use GNATLLVM.Wrapper;

package body GNATLLVM.Records.Field_Ref is

   ------------------------------
   -- RI_To_Struct_Field_Array --
   ------------------------------

   function RI_To_Struct_Field_Array
     (Ridx : Record_Info_Id) return Struct_Field_Array
   is
      package Fields is new Table.Table
        (Table_Component_Type => Struct_Field,
         Table_Index_Type     => Nat,
         Table_Low_Bound      => 1,
         Table_Initial        => 20,
         Table_Increment      => 5,
         Table_Name           => "Fields");

      Last_Ord : Int                  := -1;
      RI       : constant Record_Info := Record_Info_Table.Table (Ridx);
      F_Idx    : Field_Info_Id        := RI.First_Field;
      FI       : Field_Info;

   begin
      --  If this doesn't contain an LLVM type, this is not a native LLVM
      --  structure, so we can't do anything.

      if No (RI.MDT) then
         return Struct_Field_Array'(1 .. 0 => <>);
      end if;

      --  Otherwise, loop through all the fields in this record. There may
      --  be multiple fields corresponding to one ordinal, so just look at
      --  one of them.

      while Present (F_Idx) loop
         FI := Field_Info_Table.Table (F_Idx);

         if FI.Field_Ordinal /= Last_Ord then
            Last_Ord := FI.Field_Ordinal;
            declare
               F_Type  : constant MD_Type := Element_Type (RI.MDT, Last_Ord);
               Offset  : constant ULL     :=
                 Get_Element_Offset (+RI.MDT, Last_Ord);
               GT      : constant GL_Type :=
                 (if Is_Bitfield_By_Rep (FI.Field) then No_GL_Type else FI.GT);

            begin
               Fields.Append ((FI.Field, Offset, F_Type, GT));
            end;
         end if;

         F_Idx := FI.Next;
      end loop;

      declare
         Result : Struct_Field_Array (1 .. Fields.Last);

      begin
         for J in Result'Range loop
            Result (J) := Fields.Table (J);
         end loop;

         return Result;
      end;
   end RI_To_Struct_Field_Array;

   --------------------
   -- Selector_Field --
   --------------------

   function Selector_Field
     (N : N_Selected_Component_Id) return Record_Field_Kind_Id
   is
      In_F  : constant Record_Field_Kind_Id := Entity (Selector_Name (N));
      R_TE  : constant Record_Kind_Id       := Full_Scope (In_F);

   begin
      --  Ensure R_TE is defined, then find the matching field

      Discard (Type_Of (R_TE));
      return Find_Matching_Field (R_TE, In_F);

   end Selector_Field;

   -------------------------
   -- Record_Field_Offset --
   -------------------------

   function Record_Field_Offset
     (V : GL_Value; Field : Record_Field_Kind_Id) return GL_Value
   is
      F_GT       : GL_Type                           := Full_GL_Type (Field);
      CRC        : constant Opt_Record_Field_Kind_Id :=
        Corresponding_Record_Component (Field);
      Our_Field  : constant Record_Field_Kind_Id     :=
        (if   No (Get_Field_Info (Field)) and then Present (CRC)
              and then Full_Etype (CRC) = Full_Etype (F_GT)
         then CRC else Field);
      Rec_Type   : constant Record_Kind_Id           := Full_Scope (Our_Field);
      Rec_GT     : constant GL_Type                  :=
        Primitive_GL_Type (Rec_Type);
      First_Idx  : constant Record_Info_Id           :=
        Get_Record_Info (Rec_Type);
      F_Idx      : Field_Info_Id                     :=
        Get_Field_Info (Our_Field);
      FI         : Field_Info;
      Our_Idx    : Record_Info_Id;
      Offset     : GL_Value;
      RI         : Record_Info;
      Result     : GL_Value;
      Result_MDT : MD_Type;

   begin
      --  If the field information isn't present, this must be because
      --  we're referencing a field that's not in this variant and hence is
      --  a constraint error. So return undefined. But first try to see
      --  if we can come up with the right field.

      if No (F_Idx) then
         pragma Assert (Has_Discriminants (Rec_Type));

         if Rec_Type /= Scope (Field) then
            F_Idx := Get_Field_Info (Find_Matching_Field (Rec_Type, Field));
         end if;

         if No (F_Idx) then
            return Get_Undef_Ref (F_GT);
         end if;
      end if;

      FI      := Field_Info_Table.Table (F_Idx);
      F_GT    := FI.GT;
      Our_Idx := FI.Rec_Info_Idx;
      Offset  := To_Bytes (Get_Record_Size_So_Far (Rec_Type, V,
                                                   Start_Idx => First_Idx,
                                                   Idx       => Our_Idx));
      RI      := Record_Info_Table.Table (Our_Idx);

      --  If this is the "_parent" field, just do a conversion so we point
      --  to that type. But add it to the LValue table in case there's
      --  a reference to its discrminant.

      if Chars (Our_Field) = Name_uParent then
         Result := Ptr_To_Ref (V, F_GT);
         Add_To_LValue_List (Result);
         return Result;

      --  If the current piece is for a variable-sized object, we offset
      --  to that object and make a pointer to its type. Otherwise,
      --  make sure we're pointing to Rec_Type.

      elsif Present (RI.GT) then
         pragma Assert (not Is_Bitfield (Field));
         Result := GEP (SSI_GL_Type, Pointer_Cast (V, A_Char_GL_Type),
                        (1 => Offset));
         Set_Alignment (Result, Nat'Min (Alignment (V), Alignment (Offset)));
         return Ptr_To_Ref (Result, F_GT);
      end if;

      --  Get the primitive form of V and make sure that it's not a fat or
      --  thin pointer, but we don't make a copy because we'll be indexing
      --  into it (and the copy would be incorrect if we want this offset
      --  as an LValue).

      Result := Get (To_Primitive (V, No_Copy => True), Any_Reference,
                    For_LHS => True);

      --  Otherwise, if this is not the first piece, we have to offset to
      --  the field (in bytes).

      if Our_Idx /= First_Idx then
         Result := Set_Alignment
           (GEP (SSI_GL_Type, Pointer_Cast (Result, A_Char_GL_Type),
                 (1 => Offset)),
            Nat'Min (Alignment (V), Alignment (Offset)));
      end if;

      --  If the type is not native, we have to convert the pointer to the
      --  type of this piece (which has no corresponding GNAT type).

      if Is_Nonnative_Type (Rec_Type) then
         Result     :=
           Ptr_To_Relationship (Result, Pointer_Type (RI.MDT), Rec_GT,
                                Reference_To_Unknown);
         Set_Unknown_MDT (Result, RI.MDT);
         Result_MDT := RI.MDT;
      else
         Result     := Convert_Ref (Result, Rec_GT);
         Result_MDT := Type_Of (Rec_GT);
      end if;

      --  Finally, do a regular GEP for the field

      Result := GEP_To_Relationship
        (F_GT,
         (if Is_Bitfield (Field) then Reference_To_Unknown else Reference),
         Result, Result_MDT,
         (1 => Const_Null_32, 2 => Const_Int_32 (ULL (FI.Field_Ordinal))));

      --  If we've set this to a an unknown reference, set the type so that
      --  we know what we're pointing to.

      Maybe_Initialize_TBAA_For_Field (Result, Field, F_GT);

      if Is_Bitfield (Field) then
         Set_Unknown_MDT (Result, Element_Type (Result_MDT, FI.Field_Ordinal));
      end if;

      return Result;

   end Record_Field_Offset;

   ---------------------------
   -- Emit_Record_Aggregate --
   ---------------------------

   function Emit_Record_Aggregate
     (N : N_Subexpr_Id; Result_So_Far : GL_Value) return GL_Value
   is
      GT   : constant GL_Type := Primitive_GL_Type (Full_GL_Type (N));
      Expr : Opt_N_Component_Association_Id;

   begin
      --  If we can use Data for the result, it means that each of its
      --  components must be just a simple component into an LLVM
      --  structure, so we just go through each of the part of the
      --  aggregate and use the offset for that field, skipping a
      --  discriminant of an unchecked union. If not, we use
      --  Record_Field_Offset to do the reference.

      Expr := First (Component_Associations (N));
      return Result : GL_Value := Result_So_Far do

         --  If we haven't already made a value, do so now. If this is a
         --  loadable type or not of dynamic size and we have a value, we
         --  start with an undef of that type. Otherwise, it's a variable
         --  of that type.

         if No (Result) then
            if (Is_Loadable_Type (GT)
                  or else (not Is_Dynamic_Size (GT)
                             and then Is_No_Elab_Needed (N)))
              and then not Is_Nonnative_Type (GT)
            then
               Result := Get_Undef (GT);
            else
               Result := Allocate_For_Type (GT, N => N);
            end if;
         end if;

         --  Now process each expression

         while Present (Expr) loop
            declare
               In_F : constant Record_Field_Kind_Id :=
                 Entity (First (Choices (Expr)));
               Val  : constant Opt_N_Subexpr_Id     := Expression (Expr);
               V    : GL_Value;
               F    : Record_Field_Kind_Id;

            begin
               if (Ekind (In_F) = E_Discriminant
                     and then Is_Unchecked_Union (GT))
                 or else Decls_Only
               then
                  if Present (Val) then
                     Discard (Emit_Expression (Val));
                  end if;

               elsif Chars (In_F) = Name_uParent then

                  --  If this is "_parent", its fields are our fields too.
                  --  Assume Expression is also an N_Aggregate.

                  pragma Assert
                    (Nkind (Expression (Expr))
                       in N_Aggregate | N_Extension_Aggregate);

                  Result := Emit_Record_Aggregate (Val, Result);

               else
                  --  We are to actually insert the field. However, if we
                  --  haven't set any information for this field, it may be
                  --  a reference to a field that will cause Constraint_Error.
                  --  If so, just don't do anything with it.

                  F := Find_Matching_Field (Full_Etype (GT), In_F);

                  if Present (Get_Field_Info (F)) then
                     V := Emit_Convert_Value (Val, Field_Type (F));
                     V := Build_Field_Store (Result, F, V);

                     if Present (V) then
                        Result := V;
                     end if;
                  else
                     --  Ensure we understand this case

                     pragma Assert (Ekind (GT) = E_Record_Subtype
                                      and then Has_Discriminants (GT)
                                      and then Ekind (F) = E_Component);
                  end if;
               end if;
            end;

            Next (Expr);
         end loop;
      end return;
   end Emit_Record_Aggregate;

   ----------------------
   -- Build_Field_Load --
   ----------------------

   function Build_Field_Load
     (In_V       : GL_Value;
      In_F       : Record_Field_Kind_Id;
      LHS        : GL_Value := No_GL_Value;
      For_LHS    : Boolean  := False;
      Prefer_LHS : Boolean  := False;
      VFA        : Boolean  := False) return GL_Value
   is
      R_GT   : constant GL_Type              := Related_Type (In_V);
      R_TE   : constant Record_Kind_Id       :=
        Record_Type_For_Field (R_GT, In_F);
      F      : constant Record_Field_Kind_Id :=
        Find_Matching_Field (R_TE, In_F);
      F_GT   : constant GL_Type              := Field_Type (F);
      V      : GL_Value                      := To_Primitive (In_V);
      Result : GL_Value;

   begin
      --  If V is Volatile_Full_Access, we have to try to load the full record
      --  into memory. If we did, and this is for an LHS, we also need to
      --  set up a writeback.

      if VFA then
         V  := Get (V, Object);

         if Is_Data (V) and For_LHS then
            V := Get (V, Any_Reference, For_LHS => True);
            Add_Write_Back (In_V, Empty, V);
         end if;
      end if;

      --  If we have something in a data form and we're not requiring or
      --  preferring an LHS, and we have information about the field, we
      --  can and should do this with an Extract_Value.

      if Is_Data (V) and then not For_LHS and then not Prefer_LHS
        and then Present (Get_Field_Info (F))
        and then not Is_Nonnative_Type (F_GT)
        and then not Is_Nonnative_Type (R_TE)
        and then (not Is_Array_Bitfield (F)
                    or else (Is_Nonsymbolic_Constant (V)
                               and then not Is_Large_Array_Bitfield (F)))
      then
         Result := Extract_Value_To_Relationship
           (F_GT, V, Field_Ordinal (F),
            (if Is_Bitfield (F) then Unknown else Data));
      else
         Result := Record_Field_Offset
           (Get (V, Any_Reference, For_LHS => True), F);
      end if;

      --  If this is the parent field, we're done

      if Chars (F) = Name_uParent then
         return Result;

      --  Check for the trivial case of a zero-length field

      elsif Known_Esize (F) and then Esize (F) = 0 then
         return (if    Is_Elementary_Type (F_GT) then Const_Null (F_GT)
                 elsif Is_Loadable_Type (F_GT)   then Get_Undef (F_GT)
                 else  Get_Undef_Ref (F_GT));

      --  If we have a bitfield, we need additional processing

      elsif Is_Bitfield (F) then
         Result := Build_Bitfield_Load
           (Normalize ((Result, Field_Type (F), +Field_Bit_Offset (F),
                        +Esize (F))),
            LHS);
      end if;

      return Result;
   end Build_Field_Load;

   ------------------
   -- Field_To_Use --
   ------------------

   function Field_To_Use
     (LHS : GL_Value; F : Record_Field_Kind_Id) return Record_Field_Kind_Id
   is
      GT      : constant GL_Type        := Related_Type (LHS);
      TE      : constant Record_Kind_Id := Record_Type_For_Field (GT, F);

   begin
      --  Ensure TE is elaborated since we may need info about this field

      Discard (Type_Of (TE));
      return Find_Matching_Field (TE, F);
   end Field_To_Use;

   -----------------------
   -- Build_Field_Store --
   -----------------------

   function Build_Field_Store
     (In_LHS : GL_Value;
      In_F   : Record_Field_Kind_Id;
      RHS    : GL_Value;
      VFA    : Boolean := False) return GL_Value
   is
      R_GT      : constant GL_Type              := Related_Type (In_LHS);
      R_TE      : constant Record_Kind_Id       :=
        Record_Type_For_Field (R_GT, In_F);
      F         : constant Record_Field_Kind_Id :=
        Find_Matching_Field (R_TE, In_F);
      F_Idx     : constant Field_Info_Id        := Get_Field_Info (F);
      FI        : constant Field_Info           :=
        Field_Info_Table.Table (F_Idx);
      F_GT      : constant GL_Type              := FI.GT;
      Idx       : constant Nat                  := FI.Field_Ordinal;
      RHS_Cvt   : constant GL_Value             := Convert_GT (RHS, F_GT);
      LHS       : GL_Value                      := In_LHS;
      Result    : GL_Value                      := No_GL_Value;

   begin
      --  First check for the trivial case of a zero-length field

      if Known_Esize (F) and then Esize (F) = 0 then
         return (if Is_Data (LHS) then LHS else No_GL_Value);

      --  If this is for a volatile full access object, load that object

      elsif VFA and then not Is_Data (LHS) then
         LHS := Get (To_Primitive (LHS), Object);
      end if;

      --  If F is a bitfield, we need to process it as such. If it's data
      --  and not an array bitfield (or the data is an undef or constant),
      --  we extract the field, call our processing function, store the
      --  field back, and return it. If it's not data, we pass the address
      --  and return our original value.

      if Is_Bitfield (F) then
         if Is_Data (LHS) and then not Is_Large_Array_Bitfield (F)
           and then (not Is_Array_Bitfield (F)
                     or else (Is_Undef_Or_Nonsymbolic_Constant (LHS)
                              and then Is_Nonsymbolic_Constant (RHS)))
         then
            declare
               Idx       : constant unsigned := Field_Ordinal (F);
               Inner_LHS : constant GL_Value :=
                 Extract_Value_To_Relationship (F_GT, LHS, Idx, Unknown);
               New_Inner : constant GL_Value :=
                 Build_Bitfield_Store
                   (RHS_Cvt, Normalize ((Inner_LHS, Field_Type (F),
                                         +Field_Bit_Offset (F), +Esize (F))));
            begin
               return Insert_Value (LHS, New_Inner, Idx);
            end;
         else
            declare
               LHS_For_Access : constant GL_Value :=
                 Get (LHS, Any_Reference, For_LHS => True);

            begin
               Build_Bitfield_Store
                 (RHS_Cvt, Normalize
                    ((Record_Field_Offset (LHS_For_Access, F),
                     Field_Type (F), +Field_Bit_Offset (F), +Esize (F))));
               return LHS_For_Access;
            end;
         end if;

      --  Handle the cases where F isn't a bitfield. If we're dealing with
      --  data, just insert the data. Otherwise, emit an assignment. We
      --  convert RHS to the type of the field except in the case where the
      --  field is an unconstrained record. In that case, the size of the
      --  field shouldn't be used for the copy since it's the maximum size
      --  of the record and hence almost always larger than the amount of
      --  data to be copied.

      else
         if Is_Data (LHS) then
            Result := Insert_Value (LHS, Get (RHS_Cvt, Data), unsigned (Idx));
         else
            Emit_Assignment (Record_Field_Offset (LHS, F), Empty,
                             (if   Is_Unconstrained_Record (F_GT) then RHS
                              else Convert_GT (RHS, F_GT)));
         end if;

         return Result;
      end if;

   end Build_Field_Store;

   -----------------------
   -- Build_Field_Store --
   -----------------------

   procedure Build_Field_Store
     (LHS  : GL_Value;
      In_F : Record_Field_Kind_Id;
      RHS  : GL_Value;
      VFA  : Boolean := False)
   is
      Result : constant GL_Value := Build_Field_Store (LHS, In_F, RHS, VFA);

   begin
      --  If we have a value, copy it back into LHS

      if Present (Result) then
         Emit_Assignment (LHS, Value => Result);
      end if;
   end Build_Field_Store;

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
         Out_BRD.LHS := Ptr_To_Relationship (Out_BRD.LHS, Void_Ptr_MD,
                                             Out_BRD.GT, Reference_To_Unknown);
         Out_BRD.LHS    := GEP_To_Relationship
           (SSI_GL_Type, Reference_To_Unknown, Out_BRD.LHS, Byte_MD,
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
      Result := Record_Field_Offset (Get (Result, Any_Reference,
                                          For_LHS => True),
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

               pragma Assert (Present (RI.MDT));

               Offset := Offset +
                 To_Bytes (Get_Record_Size_So_Far (TE, Result,
                                                   Start_Idx => R_Ridx,
                                                   Idx       => F_Ridx)) +
                 Size_Const_Int (Get_Element_Offset (+RI.MDT,
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
         else Nat (Get_Scalar_Bit_Size (BRD.LHS)));
      Result      : GL_Value         := BRD.LHS;

   begin
      --  If we have constant data for a bitfield. convert it to an integer
      --  type of the same width as the input. If it's already of that
      --  type, this is a noop.

      if Relationship (Result) = Unknown and then Is_Constant (Result) then
         Result := G (Convert_Aggregate_Constant (+Result, Int_Ty (Our_Bits)),
                      F_GT, Int_Ty (Our_Bits), Unknown);
      end if;

      --  LLVM IR's load and store instructions are only defined when they're
      --  loading an integral number of bytes. We computed above the minimum
      --  number of bits needed to load all the bytes containg the needed
      --  bitfield. If we're not emitting C or if there's an integer type
      --  wide enough to represent that number of bytes, we can load those
      --  bytes, shift the data to the low-order bits, and truncate.

      if not Emit_C or else Our_Bits <= Max_Int_Size then
         declare
            MDT : constant MD_Type :=
              (if   Is_Reference (Result) then Int_Ty (Needed_Bits)
               else Type_Of (Result));

         begin
            --  If we have data, we have the entire bitfield. So all we have
            --  to do is shift. If not, we have to pick up the precise
            --  location within the field.

            if Is_Reference (Result) then
               Result := Ptr_To_Relationship (Result, Pointer_Type (MDT),
                                              Reference_To_Unknown);

               Set_Unknown_MDT (Result, MDT);
               Result := Get (Result, Unknown);
            end if;

            --  Now shift and truncate, if necessary

            if First_Bit /= 0 then
               Result := L_Shr (Result,
                                G (Const_Int (+MDT, ULL (First_Bit), False),
                                   F_GT, MDT, Unknown));
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
            High_Ptr    : constant GL_Value :=
              Convert_Ref (High_Addr, SSI_GL_Type);
            Byte        : constant GL_Value := Load (High_Ptr);
            High_Part   : constant GL_Value :=
              Z_Ext_To_Relationship (Byte, Max_Int_MD, Unknown);
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
              Ptr_To_Relationship (Memory, Pointer_Type (Type_Of (Result)),
                                   F_GT, Reference_To_Unknown);

         begin
            Set_Unknown_MDT (Mem_As_Int_Ptr, Type_Of (Related_Type (Result)));
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
         else Nat (Get_Scalar_Bit_Size ((BRD.LHS))));
      F_MDT       : constant MD_Type := Int_Ty (F_Bits);
      New_F_MDT   : constant MD_Type := Int_Ty (Num_Bits);
      New_RHS     : GL_Value         := Convert_GT (RHS, F_GT);

   begin
      --  We start by making sure that RHS is an integer. If it's a floating
      --  point or access type, we bitcast to an integer of the same width.

      if Is_Floating_Point_Type (F_GT) then
         New_RHS := Bit_Cast_To_Relationship (New_RHS, New_F_MDT, Unknown);
      elsif Is_Access_Type (F_GT) then
         New_RHS := Ptr_To_Int_To_Relationship (New_RHS, New_F_MDT, Unknown);

         --  Otherwise, unless it's an integral type, we need to either
         --  pointer-pun it into an integral type or convert it if it's a
         --  constant.

      elsif not Is_Integer_Type (F_GT) and then not Is_Enumeration_Type (F_GT)
      then
         if Is_Nonsymbolic_Constant (New_RHS) then
            New_RHS := G (Convert_Aggregate_Constant (+New_RHS, +F_MDT),
                          F_GT, F_MDT, Unknown);
         else
            New_RHS := Get (New_RHS, Reference, For_LHS => True);
            New_RHS := Ptr_To_Relationship (New_RHS, Pointer_Type (F_MDT),
                                            Reference_To_Unknown);
            Set_Unknown_MDT (New_RHS, F_MDT);
         end if;
      end if;

      --  Now ensure RHS is loaded and truncate the RHS to the number of
      --  bits corresponding to the field.

      if Is_Reference (New_RHS) then
         New_RHS := Load (New_RHS);
      end if;

      New_RHS := Trunc_To_Relationship (New_RHS, New_F_MDT, Unknown);

      --  Similarly to the load case, if we're not emitting C or if
      --  there's an integer type wide enough to represent that number of
      --  bytes, we can load those bytes, shift the RHS to the proper place,
      --  mask out those bits, and "or" in the RHS.

      if not Emit_C or else Our_Bits <= Max_Int_Size then
         declare
            Orig_MDT    : constant MD_Type  := Type_Of (BRD.LHS);
            MDT         : constant MD_Type  := Int_Ty (Our_Bits);
            Shift_Count : constant GL_Value :=
              G (Const_Int (+MDT, ULL (First_Bit), False), F_GT, MDT, Unknown);
            Ones        : constant GL_Value :=
              Z_Ext_To_Relationship (G (Const_Ones (+New_F_MDT), F_GT,
                                        New_F_MDT, Unknown),
                                     MDT, Unknown);
            Mask        : constant GL_Value := Shl (Ones, Shift_Count);
            Ext_RHS     : constant GL_Value :=
              Z_Ext_To_Relationship (New_RHS, MDT, Unknown);
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

               LHS_Ptr := Ptr_To_Relationship (LHS, Pointer_Type (MDT),
                                               Reference_To_Unknown);
               Set_Unknown_MDT (LHS_Ptr, MDT);
               LHS := Load (LHS_Ptr);
            elsif Is_Undef (LHS) then
               LHS := G (Const_Null (MDT), F_GT, MDT, Unknown);
            elsif Is_Constant (LHS) then
               LHS := G (Convert_Aggregate_Constant (+LHS, +MDT), F_GT, MDT,
                         Unknown);
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
               if Type_Of (LHS) /= Orig_MDT then
                  if Is_Undef (LHS) then
                     LHS := G (Get_Undef (Orig_MDT), F_GT, Orig_MDT, Unknown);
                  else
                     LHS := G (Convert_Aggregate_Constant (+LHS, +Orig_MDT),
                               F_GT, Orig_MDT, Unknown);
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
               else Convert_Ref (BRD.LHS, Max_Int_GL_Type));
            High_Addr   : constant GL_Value :=
              (if   Bytes_Big_Endian then Convert_Ref (BRD.LHS, SSI_GL_Type)
               else GEP (A_Char_GL_Type, BRD.LHS,
                         (1 => To_Bytes (Get_Type_Size (+New_F_MDT)))));
            Low_Ptr     : constant GL_Value :=
              Convert_Ref (Low_Addr, Max_Int_GL_Type);
            High_Ptr    : constant GL_Value :=
              Convert_Ref (High_Addr, SSI_GL_Type);
            Shift_Cnt_L : constant GL_Value :=
              G (Const_Int (Max_Int_T, ULL (First_Bit), False), F_GT,
                 Max_Int_MD, Unknown);
            Shift_Cnt_H : constant GL_Value :=
              G (Const_Int (Max_Int_T, ULL (Max_Int_Size - First_Bit), False),
                 F_GT, Max_Int_MD, Unknown);
            Ones        : constant GL_Value :=
              Z_Ext_To_Relationship (G (Const_Ones (+New_F_MDT),
                                        F_GT, New_F_MDT, Unknown),
                                     New_F_MDT, Unknown);
            Low_Mask    : constant GL_Value := Shl (Ones, Shift_Cnt_L);
            High_Mask   : constant GL_Value :=
              Trunc_To_Relationship (L_Shr (Ones, Shift_Cnt_H), Byte_MD,
                                     Unknown);
            Low_Data    : constant GL_Value := Shl (New_RHS, Shift_Cnt_L);
            High_Data   : constant GL_Value :=
              Trunc_To_Relationship (L_Shr (New_RHS, Shift_Cnt_H), Byte_MD,
                                     Unknown);
            Low_Part    : GL_Value          := Load (Low_Ptr);
            High_Part   : GL_Value          := Load (High_Ptr);

         begin
            --  Verify that the field type is the maximum integer type
            --  since the calculations above depend on it, do the logical
            --  operations, and store the values back.

            pragma Assert (New_F_MDT = Max_Int_MD);
            if Low_Mask /= Low_Data then
               Low_Part := Build_And (Low_Part, Build_Not (Low_Mask));
            end if;

            if High_Mask /= High_Data then
               High_Part := Build_And (High_Part, Build_Not (High_Mask));
            end if;

            Store (Build_Or (Low_Part,  Low_Data),  Low_Ptr);
            Store (Build_Or (High_Part, High_Data), High_Ptr);
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
