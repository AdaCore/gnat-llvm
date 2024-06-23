------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2023, AdaCore                     --
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

with Atree;      use Atree;
with Uintp.LLVM; use Uintp.LLVM;

with GNATLLVM.Codegen; use GNATLLVM.Codegen;
with GNATLLVM.Types;   use GNATLLVM.Types;
with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

with CCG.Codegen;      use CCG.Codegen;
with CCG.Environment;  use CCG.Environment;
with CCG.Instructions; use CCG.Instructions;
with CCG.Output;       use CCG.Output;
with CCG.Target;       use CCG.Target;

package body CCG.Aggregates is

   --  This package contains routines used to process aggregate data,
   --  which are arrays and structs.

   function Value_Piece (V : Value_T; T : in out Type_T; Idx : Nat) return Str
     with Pre  => Get_Opcode (V) in Op_Extract_Value | Op_Insert_Value
                  and then Is_Aggregate_Type (T),
          Post => Present (Value_Piece'Result) and then T /= T'Old;
   --  T is the type of a component of the aggregate in an extractvalue or
   --  insertvalue instruction V. Return an Str saying how to access that
   --  component and update T to be the type of that component.

   Needed_IXX_Structs : array (Nat range 1 .. 128) of Boolean :=
     (others => False);
   --  Indicates for which integer types we need to generate a struct
   --  to handle loads and stores of that type.

   ---------------------
   -- Need_IXX_Struct --
   ---------------------

   procedure Need_IXX_Struct (J : Nat) is
   begin
      Needed_IXX_Structs (J) := True;
   end Need_IXX_Struct;

   ------------------------
   -- Output_IXX_Structs --
   ------------------------

   procedure Output_IXX_Structs is
   begin
      for J in Needed_IXX_Structs'Range loop
         if Needed_IXX_Structs (J) then
            if Pack_Via_Pragma then
               Output_Decl ("#pragma pack(push, 1)",
                            Is_Typedef  => True,
                            Semicolon   => False,
                            Indent_Type => Left);
            end if;

            Output_Decl ("struct ccg_i" & J,
                         Semicolon => False, Is_Typedef => True);
            Start_Output_Block (Decl);
            Output_Decl ("unsigned " &
                         (if J > Int_Size then "long long" else "int") &
                         " f : " & J,
                         Is_Typedef => True);
            Output_Decl ("}" &
                         (if   Pack_Via_Modifier
                          then Output_Modifier ("packed", Before) else +""),
                         Is_Typedef => True, End_Block => Decl);

            if Pack_Via_Pragma then
               Output_Decl ("#pragma pack(pop)",
                            Is_Typedef  => True,
                            Semicolon   => False,
                            Indent_Type => Left);
            end if;

            Output_Decl ("", Semicolon => False, Is_Typedef => True);
         end if;
      end loop;
   end Output_IXX_Structs;

   -----------------------
   -- Default_Alignment --
   -----------------------

   function Default_Alignment (T : Type_T) return Nat is
   begin
      --  As documented in the spec of this package, we have three cases,
      --  arrays, structs, and non-composite types

      if Is_Array_Type (T) then
         return Default_Alignment (Get_Element_Type (T));

      elsif Is_Struct_Type (T) then
         declare
            Types : constant Nat := Count_Struct_Element_Types (T);

         begin
            return Max_Align : Nat := BPU do
               for J in 0 .. Types - 1 loop
                  Max_Align :=
                    Nat'Max (Actual_Alignment
                               (Struct_Get_Type_At_Index (T, J)),
                             Max_Align);
               end loop;
            end return;
         end;
      else
         return Get_Preferred_Type_Alignment (T);
      end if;

   end Default_Alignment;

   ----------------------
   -- Struct_Out_Style --
   ----------------------

   function Struct_Out_Style (T : Type_T) return Struct_Out_Style_T is
      Num_Types : constant Nat              := Count_Struct_Element_Types (T);
      F0        : constant Entity_Id        :=
        (if Num_Types = 0 then Empty else Get_Field_Entity (T, 0));
      TE        : constant Opt_Type_Kind_Id :=
        (if   Present (F0) then Full_Base_Type (Full_Etype (Scope (F0)))
         else Empty);
      Cur_Pos   : ULL                       := 0;
      Need_Pack : Boolean                   := False;
      Need_Pad  : Boolean                   := False;

   begin
      --  If this is an opaque type, we're not going to be referencing it
      --  directly, and certainly aren't going to be outputting fields for
      --  it, so we can pretend it's normal.

      if Is_Opaque_Struct (T) then
         return Normal;

      --  If this isn't a packed struct, we don't need packing. Likewise if
      --  there are no fields. But we don't know that we can omit padding
      --  fields.

      elsif not Is_Packed_Struct (T) or else Num_Types = 0 then
         return Padding;

      --  If the user asked to prefer packing, do so

      elsif Prefer_Packed then
         Need_Pack := True;

      --  ??? For now, if optimizing, we need to include padding
      --  fields since LLVM's SROA may try to preserve padding fields.

      elsif Code_Opt_Level > 0 then
         Need_Pad := True;

      end if;

      --  Now we look at the position of each field relative to its default
      --  position. Skip padding fields when doing this.

      for J in 0 .. Num_Types - 1 loop
         declare
            Actual_Pos : constant ULL    :=
              To_Bits (Get_Element_Offset (T, J));
            Elem_T     : constant Type_T := Struct_Get_Type_At_Index (T, J);
            Size       : constant ULL    := Get_Type_Size (Elem_T);
            Align      : constant ULL    := ULL (Actual_Alignment (Elem_T));
            SB_Pos     : constant ULL    :=
              (Cur_Pos + Align - 1) / Align * Align;

         begin
            if not Is_Field_Padding (T, J) then
               if Actual_Pos < SB_Pos then
                  Need_Pack := True;
               elsif Actual_Pos > SB_Pos then
                  Need_Pad  := True;
               end if;

               Cur_Pos := Cur_Pos + Size;
            end if;

            --  If we have a zero-length array that isn't the last field,
            --  which is a very rare case, we reference that field by taking
            --  the address of the next one, so we need to be conservative
            --  here.

            if Is_Zero_Length_Array (Elem_T) and then J /= Num_Types - 1 then
               Need_Pack := True;
            end if;
         end;
      end loop;

      --  If we've already determined that we need packing but we don't
      --  have a way of indicating that to the C compiler, give an error.

      if Need_Pack and then Pack_Not_Supported then
         Error_Msg ("C compiler does not support packing", T);
      end if;

      --  If we can't determine the base type, its base type is
      --  unconstrained (see the discussion in GNATLLVM.Records.Create for
      --  the rationale of this test), or if the alignment of the struct
      --  is smaller that the default alignment, we must pack. We also
      --  need to pack if the total size of the fields isn't a multiple of
      --  the alignment and this record is used as the type of a field in
      --  another record.

      if (No (TE) or else not Is_Constrained (TE)
          or else (+Alignment (TE)) * UBPU < ULL (Default_Alignment (T))
          or else (Cur_Pos mod ((+Alignment (TE)) * UBPU) /= 0
                   and then Get_Used_In_Struct (T)))
        and then not Need_Pack
      then
         Need_Pack := True;

         if Pack_Not_Supported then
            Set_Cannot_Pack (T);
         end if;
      end if;

      --  If the last field is a padding field, it's there to increase
      --  the size of the struct to match the alignment and so must always
      --  be there.

      if Is_Field_Padding (T, Num_Types - 1) then
         Need_Pad := True;
      end if;

      --  Now return what we've computed above

      return (if   Need_Pack then Packed elsif Need_Pad then Padding
              else Normal);

   end Struct_Out_Style;

   --------------------------
   -- Error_If_Cannot_Pack --
   --------------------------

   procedure Error_If_Cannot_Pack (T : Type_T) is
   begin
      --  We want this error to be output only once for a type. If the
      --  type corresponds to a GNAT type, the handling of error messages
      --  will guarantee that. If not, we may be outputting duplicates,
      --  but let's assume that's a rare case and not add a bit just for
      --  that purpose.

      if Get_Cannot_Pack (T) then
         Error_Msg ("unsupported use when packing not available", T);
      end if;
   end Error_If_Cannot_Pack;

   ---------------------------
   -- Output_Struct_Typedef --
   ---------------------------

   procedure Output_Struct_Typedef (T : Type_T; Incomplete : Boolean := False)
   is
      Num_Types      : constant Nat                :=
        Count_Struct_Element_Types (T);
      TE             : constant Opt_Type_Kind_Id   := Get_Entity (T);
      Is_Vol         : constant Boolean            :=
        Present (TE) and then Treat_As_Volatile (TE);
      Vol_Str        : constant String             :=
        (if Is_Vol then "volatile " else "");
      SOS            : constant Struct_Out_Style_T := Struct_Out_Style (T);
      Fields_Written : Nat                         := 0;
   begin
      --  Because this struct may contain a pointer to itself, we always have
      --  to write an incomplete struct. So we write, e.g.,
      --
      --       typedef struct foo foo;
      --       struct foo { ... full definition ..}

      if not Get_Is_Incomplete_Output (T) then
         Output_Decl ("typedef " & Vol_Str & "struct " & T & " " & T,
                      Is_Typedef => True);
         Set_Is_Incomplete_Output (T);
      end if;

      --  If all we're to do is to output the incomplete definition,
      --  we're done.

      if Incomplete then
         return;
      end if;

      --  Before we output the typedef for this struct, make sure we've
      --  output any inner typedefs.

      for J in 0 .. Num_Types - 1 loop
         Maybe_Output_Typedef (Struct_Get_Type_At_Index (T, J));
      end loop;

      --  Now that we know that all inner typedefs have been output,
      --  we output the struct definition.

      if Pack_Via_Pragma and then SOS = Packed then
         Output_Decl ("#pragma pack(push, 1)",
                      Is_Typedef  => True,
                      Semicolon   => False,
                      Indent_Type => Left);
      end if;

      Output_Decl ("struct " & T, Semicolon => False, Is_Typedef => True);
      Start_Output_Block (Decl);
      for J in 0 .. Num_Types - 1 loop
         declare
            ST : constant Type_T  := Struct_Get_Type_At_Index (T, J);

         begin
            Error_If_Cannot_Pack (ST);

            if not Is_Zero_Length_Array (ST)
              and then not (SOS = Normal and then Is_Field_Padding (T, J))
            then
               declare
                  Name           : constant Str                      :=
                    Get_Field_Name (T, J);
                  F              : constant Opt_Record_Field_Kind_Id :=
                    Get_Field_Entity (T, J);
                  F_Is_Vol       : constant Boolean                  :=
                    Present (F) and then Treat_As_Volatile (F)
                    and then not Is_Vol;

               begin
                  Output_Decl
                    ((ST or F) & (if F_Is_Vol then " volatile" else "") &
                      " " & Name,
                     Is_Typedef => True);
                  Fields_Written := Fields_Written + 1;
               end;
            end if;
         end;
      end loop;

      --  If this is an empty struct, we need to add a dummy field since
      --  ISO C89 doesn't allow an empty struct.

      if Fields_Written = 0 then
         Output_Decl ((if Use_Stdint then "int8_t" else "signed char") &
                        " dummy_for_null_recordC", Is_Typedef => True);
      end if;

      --  End the decl and deal with any packing

      Output_Decl ("}" &
                   (if   Pack_Via_Modifier and then SOS = Packed
                    then Output_Modifier ("packed", Before) else +""),
                   Is_Typedef => True, End_Block => Decl);

      if Pack_Via_Pragma and then SOS = Packed then
         Output_Decl ("#pragma pack(pop)",
                      Is_Typedef  => True,
                      Semicolon   => False,
                      Indent_Type => Left);
      end if;
   end Output_Struct_Typedef;

   -------------------------
   -- Write_Array_Typedef --
   -------------------------

   procedure Output_Array_Typedef (T : Type_T) is
      Elem_T : Type_T := Get_Element_Type (T);
      Decl   : Str    := +"typedef ";
   begin
      --  If Elem_T is an array that we'll write with indefinite dimensions,
      --  go down into its element type.

      while Is_Array_Type (Elem_T) and then Effective_Array_Length (Elem_T) = 0
      loop
         Elem_T := Get_Element_Type (Elem_T);
      end loop;

      --  Now build the declaration

      Maybe_Output_Typedef (Elem_T);
      Error_If_Cannot_Pack (Elem_T);
      Decl := Decl & Elem_T & " " & T & "[";

      if Effective_Array_Length (T) /= 0 then
         Decl := Decl & Effective_Array_Length (T);
      end if;

      --  Finally, output it

      Output_Decl (Decl & "]", Is_Typedef => True);
   end Output_Array_Typedef;

   ---------------------------------------
   -- Maybe_Output_Array_Return_Typedef --
   ---------------------------------------

   procedure Maybe_Output_Array_Return_Typedef (T : Type_T) is
   begin
      --  If we haven't written this yet, first ensure that we've written
      --  the typedef for T since we reference it, then write the actual
      --  typedef, and mark it as written.

      if not Get_Is_Return_Typedef_Output (T) then
         Maybe_Output_Typedef (T);
         Output_Decl ("typedef struct " & T & "_R {" & T & " F;} " & T & "_R",
                      Is_Typedef => True);
         Set_Is_Return_Typedef_Output (T);
      end if;
   end Maybe_Output_Array_Return_Typedef;

   -----------------
   -- Value_Piece --
   -----------------

   function Value_Piece
     (V : Value_T; T : in out Type_T; Idx : Nat) return Str is
   begin
      return Result : Str do
         declare
            Ins_Idx : constant Nat := Get_Index (V, Idx);
         begin
            --  We know this is either a struct or an array

            if Is_Struct_Type (T) then
               Result := "." & Get_Field_Name (T, Ins_Idx) + Component;
               T      := Struct_Get_Type_At_Index (T, Ins_Idx);
            else
               Result := " [" & Ins_Idx & "]" + Component;
               T      := Get_Element_Type (T);
            end if;
         end;
      end return;
   end Value_Piece;

   -------------------------------
   -- Extract_Value_Instruction --
   -------------------------------

   function Extract_Value_Instruction (V : Value_T; Op : Value_T) return Str is
      Idxs : constant Nat := Get_Num_Indices (V);
      T    : Type_T       := Type_Of (Op);
   begin
      return Result : Str := Op + Component do

         --  We process each index in turn, stripping off the reference.

         for J in 0 .. Idxs - 1 loop
            Result := Result & Value_Piece (V, T, J);
         end loop;
      end return;
   end Extract_Value_Instruction;

   ------------------------------
   -- Insert_Value_Instruction --
   ------------------------------

   procedure Insert_Value_Instruction (V, Aggr, Op : Value_T) is
      Idxs : constant Nat := Get_Num_Indices (V);
      T    : Type_T       := Type_Of (Aggr);
      Acc  : Str          := +V;

   begin
      --  If Aggr is undef, we don't need to do any copy. Otherwise, we
      --  first copy it to the result variable.

      Maybe_Decl (V);

      if Is_Undef (Aggr) then
         null;
      else
         Output_Copy (V, +Aggr, T);
      end if;

      --  Next we generate the string that represents the access of this
      --  instruction.

      for J in 0 .. Idxs - 1 loop
         Acc := Acc & Value_Piece (V, T, J);
      end loop;

      --  The resulting type must be that of Op and we emit the assignment

      pragma Assert (T = Type_Of (Op));
      Output_Copy (Acc, Op + Assign, T, V);
   end Insert_Value_Instruction;

   ---------------------
   -- GEP_Instruction --
   ---------------------

   procedure GEP_Instruction (V : Value_T; Ops : Value_Array) is
      Aggr   : constant Value_T := Ops (Ops'First);
      --  The pointer to aggregate that we're dereferencing

      Aggr_T : Type_T           := Get_Element_Type (Aggr);
      --  The type that Aggr, which is always a pointer, points to

      Is_LHS : Boolean          := Get_Is_LHS (Aggr);
      --  Whether our result so far is an LHS as opposed to a pointer.
      --  If it is, then we can use normal derefrence operations and we must
      --  take the address at the end of the instruction processing.

      Result : Str;
      --  The resulting operation so far

   begin
      --  The first operand is special in that it represents a value to be
      --  multiplied by the size of the type pointed to and added to the
      --  value of the pointer input. Normally, we have a GEP that either
      --  has a nonzero value for this operand and no others or that has a
      --  zero for this value, but those aren't requirements. However, it's
      --  very worth special-casing the zero case here because we have
      --  nothing to do in that case.

      if Is_A_Constant_Int (Ops (Ops'First + 1))
        and then Equals_Int (Ops (Ops'First + 1), 0)
      then
         --  If we have a NULL, we need to include the type so that C
         --  can properly interpret it.

         if Is_A_Constant_Pointer_Null (Aggr) or else Is_Undef (Aggr) then
            Result := TP ("((volatile #T1) #1)", Aggr) + Component;
            Is_LHS := True;
         else
            Result := Aggr + LHS + Component;
         end if;
      elsif Is_A_Constant_Pointer_Null (Aggr) or else Is_Undef (Aggr) then
         Result := TP ("((volatile #T1) #1)[#P2]", Aggr,
                       Ops (Ops'First + 1)) + Component;
         Is_LHS := True;

      else
         Result := TP ("#1[#P2]", Aggr, Ops (Ops'First + 1)) + Component;
         Is_LHS := True;
      end if;

      --  Now process any other operands, which must always dereference into
      --  an array or struct. When we make a component reference of an object,
      --  we must ensure that the actual type of the object, not just a pointer
      --  to that object, will have been fully defined and isn't an incomplete
      --  type.

      for Op of Ops (Ops'First + 2 .. Ops'Last) loop
         Maybe_Output_Typedef (Aggr_T);

         if Is_Array_Type (Aggr_T) then

            --  If this isn't an LHS, we have to make it one

            if not Is_LHS then
               Result := Deref (Result) + Component;
            end if;

            Result := Result & TP ("[#P1]", Op) + Component;
            Aggr_T := Get_Element_Type (Aggr_T);
            Is_LHS := True;

         else
            pragma Assert (Is_Struct_Type (Aggr_T));

            declare
               Idx   : constant Nat    := Nat (Const_Int_Get_S_Ext_Value (Op));
               ST    : constant Type_T :=
                 Struct_Get_Type_At_Index (Aggr_T, Idx);
               Found : Boolean         := False;

            begin
               --  If this is a zero-length array, it may not actually
               --  exist. If it doesn't, convert this into a cast to char *,
               --  point past the end of a previous non-zero-length-array
               --  field (or at the start of the struct if none) and then
               --  cast to a pointer to the array's element type.

               if Is_Zero_Length_Array (ST) then
                  for Prev_Idx in reverse 0 .. Idx - 1 loop
                     declare
                        Prev_ST : constant Type_T :=
                          Struct_Get_Type_At_Index (Aggr_T, Prev_Idx);
                        Ref     : constant Str    :=
                          Result & (if Is_LHS then "." else "->") +
                            Component & Get_Field_Name (Aggr_T, Prev_Idx);

                     begin
                        --  If we found a previous non-zero-length array
                        --  field, point to the end of it.

                        if not Is_Zero_Length_Array (Prev_ST) then
                           Result :=
                             "(" & Generic_Ptr & ")" & Addr_Of (Ref) &
                             " + sizeof (" & Ref & ")";
                           Found  := True;
                           exit;
                        end if;
                     end;
                  end loop;

                  --  If we haven't found such a field, point to the beginning
                  --  of the object.

                  if not Found then
                     Result := "(" & Generic_Ptr & ")" &
                       (if Is_LHS then Addr_Of (Result) else Result);
                  end if;

                  --  Now cast to the desired type

                  Result := "((" & ST & " *) (" & Result & "))";
                  Is_LHS := False;

               --  Otherwise, just do a normal field reference

               else
                  Result :=
                    Result & (if Is_LHS then "." else "->") + Component &
                      Get_Field_Name (Aggr_T, Idx);
                  Is_LHS := True;
               end if;

               Aggr_T := ST;
            end;
         end if;
      end loop;

      --  If the input is a constant, mark the output as constant and
      --  as the value of V, mark as LHS if it is,a and we're done.

      if Get_Is_Constant (Aggr) then
         Set_Is_Constant (V);
         Set_Is_LHS (V, Is_LHS);
         Set_C_Value (V, Result);
         return;
      end if;

      --  If we ended up with a LHS, we set this as the value of V but mark
      --  it as an LHS. This is to avoid taking an address and then doing a
      --  dereference for nested GEP's.

      Set_Is_LHS (V, Is_LHS);
      if Is_LHS then

         --  If we have more than one uses and side-effects (e.g., an
         --  alloca), we can't do this. So remove the LHS indication,
         --  explicitly take the address, and process as a normal assignment.

         if Num_Uses (V) > 1 and then Has_Side_Effects (V) then
            Set_Is_LHS (V, False);
            Assignment (V, Addr_Of (Result));
         else
            Set_C_Value (V, Result);
         end if;
      else
         Assignment (V, Result);
      end if;

   end GEP_Instruction;

end CCG.Aggregates;
