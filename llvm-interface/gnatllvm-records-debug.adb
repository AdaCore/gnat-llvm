------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                  Copyright (C) 2013-2025, AdaCore                        --
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

with Ada.Containers.Vectors;

with Nlists;     use Nlists;
with Sem_Aux;    use Sem_Aux;
with Sinput;     use Sinput;
with Uintp.LLVM; use Uintp.LLVM;

with GNATLLVM.DebugInfo;   use GNATLLVM.DebugInfo;
with GNATLLVM.Helper;      use GNATLLVM.Helper;
with GNATLLVM.Utils;       use GNATLLVM.Utils;
with GNATLLVM.Wrapper;     use GNATLLVM.Wrapper;

with LLVM.Debug_Info; use LLVM.Debug_Info;

package body GNATLLVM.Records.Debug is

   subtype DWARF_OP_Encoding_T is uint64_t;

   --  DWARF expression constants.  These come from the DWARF standard
   --  and won't change.
   DW_OP_deref               : constant DWARF_OP_Encoding_T := 16#06#;
   DW_OP_constu              : constant DWARF_OP_Encoding_T := 16#10#;
   DW_OP_consts              : constant DWARF_OP_Encoding_T := 16#11#;
   DW_OP_over                : constant DWARF_OP_Encoding_T := 16#14#;
   DW_OP_swap                : constant DWARF_OP_Encoding_T := 16#16#;
   DW_OP_rot                 : constant DWARF_OP_Encoding_T := 16#17#;
   DW_OP_abs                 : constant DWARF_OP_Encoding_T := 16#19#;
   DW_OP_and                 : constant DWARF_OP_Encoding_T := 16#1a#;
   DW_OP_div                 : constant DWARF_OP_Encoding_T := 16#1b#;
   DW_OP_minus               : constant DWARF_OP_Encoding_T := 16#1c#;
   DW_OP_mod                 : constant DWARF_OP_Encoding_T := 16#1d#;
   DW_OP_mul                 : constant DWARF_OP_Encoding_T := 16#1e#;
   DW_OP_neg                 : constant DWARF_OP_Encoding_T := 16#1f#;
   DW_OP_not                 : constant DWARF_OP_Encoding_T := 16#20#;
   DW_OP_or                  : constant DWARF_OP_Encoding_T := 16#21#;
   DW_OP_plus                : constant DWARF_OP_Encoding_T := 16#22#;
   DW_OP_plus_uconst         : constant DWARF_OP_Encoding_T := 16#23#;
   DW_OP_shl                 : constant DWARF_OP_Encoding_T := 16#24#;
   DW_OP_shr                 : constant DWARF_OP_Encoding_T := 16#25#;
   DW_OP_shra                : constant DWARF_OP_Encoding_T := 16#26#;
   DW_OP_xor                 : constant DWARF_OP_Encoding_T := 16#27#;
   DW_OP_eq                  : constant DWARF_OP_Encoding_T := 16#29#;
   DW_OP_ge                  : constant DWARF_OP_Encoding_T := 16#2a#;
   DW_OP_gt                  : constant DWARF_OP_Encoding_T := 16#2b#;
   DW_OP_le                  : constant DWARF_OP_Encoding_T := 16#2c#;
   DW_OP_lt                  : constant DWARF_OP_Encoding_T := 16#2d#;
   DW_OP_ne                  : constant DWARF_OP_Encoding_T := 16#2e#;
   DW_OP_lit0                : constant DWARF_OP_Encoding_T := 16#30#;
   DW_OP_deref_size          : constant DWARF_OP_Encoding_T := 16#94#;
   DW_OP_push_object_address : constant DWARF_OP_Encoding_T := 16#97#;

   type Dwarf_Expression is array (Nat range <>) of aliased uint64_t;
   --  In LLVM, a DWARF expression is constructed from an array of
   --  uint64_t.

   package Member_Vectors is new
     Ada.Containers.Vectors (Index_Type => Pos,
                             Element_Type => Metadata_T);
   --  A vector of LLVM metadata, used when building the fields for a
   --  type or a variant part.

   subtype Member_Vector is Member_Vectors.Vector;
   --  Type of vector of members.

   function Convert_Choices (Choices : List_Id) return Word_Array;
   --  Convert a list of choices (the discrete choices selecting a
   --  variant part) to a word array.  The resulting word array has an
   --  even number of entries, with each pair holding the low and high
   --  bounds of a given choice.  Returns an empty array for 'others'
   --  or if the choice list is not recognized in some way.

   package Dw_Vector is new Ada.Containers.Vectors (Index_Type => Nat,
                                                    Element_Type => uint64_t);
   --  Used when building a DWARF expression, which is just a vector
   --  of uint64_t; this particular representation is chosen because
   --  it is convenient to pass to LLVM.

   function Find_Discriminant_From_Index (Original_Type : Entity_Id;
                                          Index : Pos) return Entity_Id;
   --  Given the index of a discriminant (like Discriminant_Number)
   --  and a type, return the corresponding field.

   function Convert_One_Field (M : in out Discriminant_Map;
                               Original_Type : Entity_Id;
                               F : Record_Field_Kind_Id;
                               Artificial : Boolean := False)
     return Metadata_T;
   --  Convert a single field to LLVM debug metadata.  M is a map to
   --  update; if the field is a discriminant, then it is recorded in
   --  the map for later lookup.  If Artificial is True, the created
   --  field is marked artificial.  Returns the LLVM debug metadata,
   --  or No_Metadata_T is the field could not be converted.

   function Convert_Variant_Part (M : in out Discriminant_Map;
                                  RI : Record_Info;
                                  Original_Type : Entity_Id;
                                  Is_Union : Boolean;
                                  Toplevel_Members : in out Member_Vector)
     return Metadata_Array
     with pre => RI.Variants /= null;
   --  Convert a variant part to LLVM debug metadata, returning an
   --  array holding the LLVM debug metadata for all the relevant
   --  fields.

   function Convert_RI_Chain (M : in out Discriminant_Map;
                              Start : Record_Info_Id;
                              Original_Type : Entity_Id;
                              Is_Union : Boolean;
                              Toplevel_Members : in out Member_Vector;
                              Outermost_Call : Boolean)
     return Metadata_Array;
   --  Convert a chain of Record_Info_Ids to LLVM debug metadata,
   --  returning an array holding the metadata for the relevant
   --  fields.  Toplevel_Members points to the vector of members for
   --  the outermost type being constructed; in some cases we may need
   --  to add a field there when processing a variant part.  This is
   --  only null for the outermost call to Convert_RI_Chain.

   function Convert_RI_Chain (M : in out Discriminant_Map;
                              Start : Record_Info_Id;
                              Original_Type : Entity_Id;
                              Is_Union : Boolean)
     return Metadata_Array;
   --  A wrapper for Convert_RI_Chain that creates the outermost
   --  member vector and passes it in.

   ---------------------
   -- Convert_Choices --
   ---------------------

   function Convert_Choices (Choices : List_Id) return Word_Array is
      package Choice_Table is new Table.Table
        (Table_Component_Type => uint64_t,
         Table_Index_Type     => Int,
         Table_Low_Bound      => 1,
         Table_Initial        => 20,
         Table_Increment      => 5,
         Table_Name           => "Choice_Table");

      Low : Uint;
      High : Uint;
      Choice : Entity_Id := First (Choices);
      Empty_Result : Word_Array (1 .. 0);
   begin
      while Present (Choice) loop
         if Nkind (Choice) = N_Others_Choice then
            return Empty_Result;
         end if;
         Decode_Range (Choice, Low, High);
         Choice_Table.Append (uint64_t (UI_To_ULL (Low)));
         Choice_Table.Append (uint64_t (UI_To_ULL (High)));
         Next (Choice);
      end loop;

      declare
         Result : Word_Array (1 .. Choice_Table.Last);
      begin
         for J in Result'Range loop
            Result (J) := Choice_Table.Table (J);
         end loop;
         return Result;
      end;
   end Convert_Choices;

   ----------------------------------
   -- Find_Discriminant_From_Index --
   ----------------------------------

   function Find_Discriminant_From_Index (Original_Type : Entity_Id;
                                          Index : Pos) return Entity_Id is
      N : Pos := 1;
      Discr : Entity_Id := First_Discriminant (Original_Type);
   begin
      while Present (Discr) loop
         if Index = N then
            return Discr;
         end if;
         N := N + 1;
         Next_Discriminant (Discr);
      end loop;
      pragma Assert (False);
   end Find_Discriminant_From_Index;

   ---------------------------------
   -- Convert_To_Dwarf_Expression --
   ---------------------------------

   function Convert_To_Dwarf_Expression (Expr : Node_Ref_Or_Val;
                                         Original_Type : Entity_Id)
     return Metadata_T
   is

      Could_Not_Convert : Boolean := False;
      --  If this expression could not be converted to a DWARF
      --  expression for some reason, this flag will be set.

      Expression : Dw_Vector.Vector;
      --  The expression being built.

      procedure Unop (Code : TCode; Op : Node_Ref_Or_Val);
      procedure Binop (Code : TCode; Lhs : Node_Ref_Or_Val;
                       Rhs : Node_Ref_Or_Val);
      procedure Cond_Expr (Test : Node_Ref_Or_Val;
                           Lhs : Node_Ref_Or_Val;
                           Rhs : Node_Ref_Or_Val);
      procedure Const (Val : Node_Ref_Or_Val);
      procedure Discriminant (Val : Node_Ref_Or_Val);
      procedure Variable (Val : Node_Ref_Or_Val);

      procedure Convert_It is new Visit (Visit_Unop => Unop,
                                         Visit_Binop => Binop,
                                         Visit_Cond_Expr => Cond_Expr,
                                         Visit_Constant => Const,
                                         Visit_Discriminant => Discriminant,
                                         Visit_Variable => Variable);
      --  Expression conversion via the visitor API.  The approach
      --  taken when generating a DWARF expression is that each
      --  visited operation leaves a single new element on the top of
      --  the stack.  That is, a callback can visit its operands and
      --  then emit code that assumes that the needed values are on
      --  the stack in the emission order.  Conversion failures are
      --  handled by setting Could_Not_Convert; in this case code may
      --  still be emitted to Expression for convenience, but at the
      --  end it should be assumed to be garbage.

      procedure Unop (Code : TCode; Op : Node_Ref_Or_Val) is
      begin
         Convert_It (Op);
         case Code is
            when Negate_Expr =>
               if DI_Expression_Extensions then
                  Expression.Append (DW_OP_neg);
               else
                  Could_Not_Convert := True;
               end if;
            when Abs_Expr =>
               if DI_Expression_Extensions then
                  Expression.Append (DW_OP_abs);
               else
                  Could_Not_Convert := True;
               end if;
            when Truth_Not_Expr =>
               Expression.Append (DW_OP_lit0);
               Expression.Append (DW_OP_eq);

            when others =>
               pragma Assert (False);
         end case;
      end Unop;

      procedure Binop (Code : TCode; Lhs : Node_Ref_Or_Val;
                       Rhs : Node_Ref_Or_Val) is
      begin
         Convert_It (Lhs);
         Convert_It (Rhs);

         case Code is
            when Plus_Expr =>
               Expression.Append (DW_OP_plus);
            when Minus_Expr =>
               Expression.Append (DW_OP_minus);
            when Mult_Expr =>
               Expression.Append (DW_OP_mul);
            when Trunc_Div_Expr | Ceil_Div_Expr | Floor_Div_Expr
                 | Exact_Div_Expr =>
               --  FIXME the kinds of div
               Expression.Append (DW_OP_div);
            when Trunc_Mod_Expr | Ceil_Mod_Expr | Floor_Mod_Expr =>
               --  This (erroneously) implements all forms of 'mod'
               --  the same way.  However note that DWARF does not
               --  clearly specify the definition of mod, so there are
               --  already debugger differences here.
               --  https://dwarfstd.org/issues/250924.2.html
               Expression.Append (DW_OP_mod);
            when Min_Expr =>
               if DI_Expression_Extensions then
                  --  LLVM can't emit branches, so we use an old HAKMEM
                  --  trick to compute a branchless minimum, namely:
                  --  MIN := Y ^ ((X ^ Y) & - (X < Y))
                  --  Stack: x y
                  Expression.Append (DW_OP_swap);
                  Expression.Append (DW_OP_over);
                  Expression.Append (DW_OP_over);
                  Expression.Append (DW_OP_over);
                  --  Stack: y x y x y
                  Expression.Append (DW_OP_lt);
                  Expression.Append (DW_OP_neg);
                  Expression.Append (DW_OP_rot);
                  --  Stack: y [-(x<y)] x y
                  Expression.Append (DW_OP_xor);
                  Expression.Append (DW_OP_and);
                  Expression.Append (DW_OP_xor);
               else
                  Could_Not_Convert := True;
               end if;

            when Max_Expr =>
               if DI_Expression_Extensions then
                  --  Similar to minimum, above.
                  --  MAX := X ^ ((X ^ Y) & - (X < Y))
                  --  Stack: x y
                  Expression.Append (DW_OP_over);
                  Expression.Append (DW_OP_swap);
                  Expression.Append (DW_OP_over);
                  Expression.Append (DW_OP_over);
                  --  Stack: x x y x y
                  Expression.Append (DW_OP_lt);
                  Expression.Append (DW_OP_neg);
                  Expression.Append (DW_OP_rot);
                  --  Stack: x [-(x<y)] x y
                  Expression.Append (DW_OP_xor);
                  Expression.Append (DW_OP_and);
                  Expression.Append (DW_OP_xor);
               else
                  Could_Not_Convert := True;
               end if;

            when Truth_And_Expr =>
               Expression.Append (DW_OP_and);
            when Truth_Or_Expr =>
               Expression.Append (DW_OP_or);
            when Truth_Xor_Expr =>
               Expression.Append (DW_OP_xor);
            when Lt_Expr =>
               Expression.Append (DW_OP_lt);
            when Le_Expr =>
               Expression.Append (DW_OP_le);
            when Gt_Expr =>
               Expression.Append (DW_OP_gt);
            when Ge_Expr =>
               Expression.Append (DW_OP_ge);
            when Eq_Expr =>
               Expression.Append (DW_OP_eq);
            when Ne_Expr =>
               Expression.Append (DW_OP_ne);
            when Bit_And_Expr =>
               Expression.Append (DW_OP_and);
            when others =>
               pragma Assert (False);
         end case;
      end Binop;

      procedure Cond_Expr (Test : Node_Ref_Or_Val;
                           Lhs : Node_Ref_Or_Val;
                           Rhs : Node_Ref_Or_Val) is
      begin
         if not DI_Expression_Extensions then
            Could_Not_Convert := True;
            return;
         end if;

         --  LLVM can't emit branches, so we use an old HAKMEM
         --  trick to compute a branchless condition, namely:
         --     C := T ? L : R
         --  Introduce M = -T (assuming T == 0 or 1)
         --     C := (L & M) | (R & ~M)

         Convert_It (Test);
         --  Just in case, let's make sure that the value is 0 or 1.
         Expression.Append (DW_OP_lit0);
         Expression.Append (DW_OP_ne);
         --  Now compute "M".
         Expression.Append (DW_OP_neg);

         Convert_It (Lhs);

         --  Stack: M L
         Expression.Append (DW_OP_over);
         Expression.Append (DW_OP_and);
         Expression.Append (DW_OP_swap);

         --  Stack: [M&L] M
         Expression.Append (DW_OP_not);
         Convert_It (Rhs);
         Expression.Append (DW_OP_and);

         --  Stack: [M&L] [~M&R]
         Expression.Append (DW_OP_or);
      end Cond_Expr;

      procedure Const (Val : Node_Ref_Or_Val) is
         Int_Val : constant Int := UI_To_Int (Val);
      begin
         if Int_Val < 0 then
            Expression.Append (DW_OP_consts);
         else
            Expression.Append (DW_OP_constu);
         end if;
         Expression.Append (uint64_t (Int_Val));
      end Const;

      procedure Discriminant (Val : Node_Ref_Or_Val) is
         F : constant Record_Field_Kind_Id :=
           Find_Discriminant_From_Index (Original_Type, UI_To_Int (Val));
         Offset_In_Bits : constant ULL := UI_To_ULL (Component_Bit_Offset (F));
         Offset_In_Bytes : constant ULL := Offset_In_Bits / UBPU;
         --  The low-order bits to shift off.
         Relative_Bit_Offset : constant ULL := Offset_In_Bits -
                                               UBPU * Offset_In_Bytes;
         Size_In_Bits : constant ULL := UI_To_ULL (Esize (F));
         Size_In_Bytes : constant ULL := (Size_In_Bits + UBPU - 1) / UBPU;
         Pointer_Bits : constant ULL := ULL (Thin_Pointer_Size);
         Is_Unsigned : constant Boolean := Is_Unsigned_Type (Etype (F));
      begin
         --  Find the location of the discriminant.
         Expression.Append (DW_OP_push_object_address);
         --  Frequently the discriminant is the first member, so avoid
         --  some extra work in this case.
         if Offset_In_Bytes /= 0 then
            Expression.Append (DW_OP_plus_uconst);
            Expression.Append (uint64_t (Offset_In_Bytes));
         end if;
         --  Extract the necessary bytes.
         if Size_In_Bytes * UBPU = Pointer_Bits then
            Expression.Append (DW_OP_deref);
         else
            Expression.Append (DW_OP_deref_size);
            Expression.Append (uint64_t (Size_In_Bytes));
         end if;

         --  For unsigned types, when there is no bit offset, some
         --  optimization is possible.
         if Is_Unsigned and then Relative_Bit_Offset = 0 then
            --  If the dereference was exactly the desired number of
            --  bits, nothing more need be done.  Otherwise, mask.
            if Size_In_Bytes * UBPU /= Size_In_Bits then
               Expression.Append (DW_OP_constu);
               Expression.Append (uint64_t (2 ** Integer (Size_In_Bits)) - 1);
               Expression.Append (DW_OP_and);
            end if;
         else
            declare
               Left_Shift : constant ULL :=
                 Pointer_Bits - (Size_In_Bits + Relative_Bit_Offset);
               Right_Shift : constant ULL := Pointer_Bits - Size_In_Bits;
            begin
               if Left_Shift /= 0 then
                  Expression.Append (DW_OP_constu);
                  Expression.Append (uint64_t (Left_Shift));
                  Expression.Append (DW_OP_shl);
               end if;
               if Right_Shift /= 0 then
                  Expression.Append (DW_OP_constu);
                  Expression.Append (uint64_t (Right_Shift));
                  Expression.Append (if Is_Unsigned
                                     then DW_OP_shr
                                     else DW_OP_shra);
               end if;
            end;
         end if;
      end Discriminant;

      procedure Variable (Val : Node_Ref_Or_Val) is
         pragma Unreferenced (Val);
      begin
         --  Not implemented yet.
         Could_Not_Convert := True;
      end Variable;

   begin
      --  If the outermost value is an ordinary constant, just return
      --  a constant.  This results in cleaner DWARF.
      if Expr >= 0 then
         return Constant_As_Metadata (Expr);
      end if;

      Convert_It (Expr);
      if Could_Not_Convert then
         return No_Metadata_T;
      end if;

      declare
         Data : aliased Dwarf_Expression (0 .. Nat (Expression.Length) - 1);
      begin
         for I in Data'Range loop
            Data (I) := Expression (I);
         end loop;

         return DI_Builder_Create_Expression (DI_Builder,
                                              Data (Data'First)'Access,
                                              Data'Length);

      end;
   end Convert_To_Dwarf_Expression;

   --------------------------------
   -- Canonical_Discriminant_For --
   --------------------------------

   function Canonical_Discriminant_For (E : Entity_Id) return Entity_Id is
      Iter : Entity_Id := Original_Record_Component (E);
   begin
      while Present (Corresponding_Discriminant (Iter)) loop
         Iter := Corresponding_Discriminant (Iter);
      end loop;
      return Iter;
   end Canonical_Discriminant_For;

   -----------------------
   -- Convert_One_Field --
   -----------------------

   function Convert_One_Field (M : in out Discriminant_Map;
                               Original_Type : Entity_Id;
                               F : Record_Field_Kind_Id;
                               Artificial : Boolean := False) return Metadata_T
   is
      F_GT           : constant GL_Type    := Field_Type (F);
      Mem_MD         : constant Metadata_T :=
        Create_Type_Data (F_GT, M'Access);
      Name           : constant String     := Get_Name (F);
      F_S            : constant Source_Ptr := Sloc (F);
      File           : constant Metadata_T :=
        Get_Debug_File_Node (Get_Source_File_Index (F_S));
      Flags          : constant DI_Flags_T :=
        (if Artificial then DI_Flag_Artificial else DI_Flag_Zero);
      Offset         : constant Metadata_T :=
        Convert_To_Dwarf_Expression (Component_Bit_Offset (F), Original_Type);
      Size           : constant Metadata_T :=
        (if Is_Bitfield (F)
         then Constant_As_Metadata (Esize (F))
         else No_Metadata_T);
      MD             : Metadata_T;
   begin
      --  Special case known static offset and size, because this will
      --  still work with an unpatched LLVM.
      if No (Mem_MD) then
         --  Nothing to do here.
         return No_Metadata_T;
      elsif Known_Static_Component_Bit_Offset (F) and then
            Known_Static_Esize (F)
      then
         declare
            Offset         : constant ULL        :=
              UI_To_ULL (Component_Bit_Offset (F));
            Storage_Offset : constant ULL        :=
              (Offset / UBPU) * UBPU;
         begin
            if Is_Bitfield (F) then
               MD := DI_Create_Bit_Field_Member_Type
                 (No_Metadata_T, Name, File,
                  Get_Physical_Line_Number (F_S),
                  UI_To_ULL (Esize (F)), Offset,
                  Storage_Offset, Mem_MD, Flags);
            else
               MD := DI_Create_Member_Type
                 (No_Metadata_T, Name, File,
                  Get_Physical_Line_Number (F_S),
                  UI_To_ULL (Esize (F)),
                  Get_Type_Alignment (F_GT), Offset, Mem_MD, Flags);
            end if;
         end;
      elsif not Types_Can_Have_Dynamic_Offsets then
         --  Unpatched LLVM.  There's nothing really sensible to do
         --  here.
         return No_Metadata_T;
      else
         MD := Create_Member (DI_Builder, No_Metadata_T, Name, File,
                              Get_Physical_Line_Number (F_S),
                              Size, Offset,
                              Mem_MD, Flags, Is_Bitfield (F));
      end if;

      --  Ensure the field is available so that a later lookup of a
      --  discriminant will succeed.
      if Ekind (F) = E_Discriminant then
         --  Should not have been seen before.
         pragma Assert (not M.Contains (Canonical_Discriminant_For (F)));
         M.Insert (Canonical_Discriminant_For (F), MD);
      end if;
      return MD;
   end Convert_One_Field;

   --------------------------
   -- Convert_Variant_Part --
   --------------------------

   function Convert_Variant_Part
     (M : in out Discriminant_Map;
      RI : Record_Info;
      Original_Type : Entity_Id;
      Is_Union : Boolean;
      Toplevel_Members : in out Member_Vector) return Metadata_Array
   is
      package Member_Table is new Table.Table
        (Table_Component_Type => Metadata_T,
         Table_Index_Type     => Int,
         Table_Low_Bound      => 1,
         Table_Initial        => 20,
         Table_Increment      => 5,
         Table_Name           => "Member_Table");

      Var_Node : Node_Id := First (RI.Variant_List);
   begin
      for J in RI.Variants'Range loop
         declare
            Empty_Fields : Metadata_Array (1 .. 0);
            Fields : constant Metadata_Array :=
              (if Present (RI.Variants (J))
               then Convert_RI_Chain (M, RI.Variants (J), Original_Type,
                                      Is_Union, Toplevel_Members, False)
               else Empty_Fields);
         begin
            if Is_Union then
               for I in Fields'Range loop
                  Member_Table.Append (Fields (I));
               end loop;
            else
               declare
                  Choices : constant Word_Array :=
                    Convert_Choices (Discrete_Choices (Var_Node));
                  MD : Metadata_T;
               begin
                  MD := Create_Variant_Member (DI_Builder, Fields, Choices);
                  Member_Table.Append (MD);
               end;
            end if;
         end;
         Next (Var_Node);
      end loop;

      --  ??? handle Overlap_Variants here.

      declare
         Members : Metadata_Array (1 .. Member_Table.Last);
      begin
         for J in Members'Range loop
            Members (J) := Member_Table.Table (J);
         end loop;
         return Members;
      end;
   end Convert_Variant_Part;

   ----------------------
   -- Convert_RI_Chain --
   ----------------------

   function Convert_RI_Chain (M : in out Discriminant_Map;
                              Start : Record_Info_Id;
                              Original_Type : Entity_Id;
                              Is_Union : Boolean;
                              Toplevel_Members : in out Member_Vector;
                              Outermost_Call : Boolean)
     return Metadata_Array
   is
      Local_Members : aliased Member_Vector;
      Members_To_Update : constant access Member_Vector :=
        (if Outermost_Call
         then Toplevel_Members'Access
         else Local_Members'Access);
      Ridx : Record_Info_Id := Start;
      RI : Record_Info;
      F : Record_Field_Kind_Id;
      F_Idx : Field_Info_Id;
      FI : Field_Info;
   begin
      --  Convert the ordinary fields.
      while Present (Ridx) loop
         RI := Record_Info_Table.Table (Ridx);
         F_Idx := RI.First_Field;
         while Present (F_Idx) loop
            FI := Field_Info_Table.Table (F_Idx);
            F := FI.Field;

            --  Skip inherited components.
            if not FI.Is_Inherited then
               declare
                  Member : constant Metadata_T :=
                    Convert_One_Field (M, Original_Type, F);
               begin
                  if Present (Member) then
                     Members_To_Update.Append (Member);
                  end if;
               end;
            elsif Ekind (F) = E_Discriminant then
               declare
                  --  However, inherited discriminants must still be
                  --  recorded in the discriminant vector.
                  Ignore : constant Metadata_T :=
                    Convert_One_Field (M, Original_Type, F, True);
               begin
                  null;
               end;
            end if;

            F_Idx := FI.Next;
         end loop;

         Ridx := RI.Next;
      end loop;

      --  Convert the variant part, if any -- but only if compiled
      --  against an LLVM that supports variant parts with multiple
      --  members.
      if Types_Can_Have_Multiple_Variant_Members and then
         RI.Variants /= null
      then
         declare
            Variant_MD : Metadata_T;
            MD : Metadata_T;
            Parts : constant Metadata_Array :=
              Convert_Variant_Part (M, RI, Original_Type, Is_Union,
                                    Toplevel_Members);
            Discrim : constant Entity_Id := Entity (RI.Variant_Expr);
         begin
            if Is_Union then
               for I in Parts'Range loop
                  Members_To_Update.Append (Parts (I));
               end loop;
            else
               if Ekind (Discrim) = E_Discriminant then
                  --  If we already processed the discriminant, just
                  --  reuse its metadata here.  However, if the
                  --  discriminant was inherited, there will not be
                  --  debuginfo for it.  In this case, we emit an
                  --  artificial discriminant into this record, so
                  --  that the variant part can refer to it.
                  if M.Contains (Canonical_Discriminant_For (Discrim)) then
                     Variant_MD := M (Canonical_Discriminant_For (Discrim));
                  else
                     Variant_MD := Convert_One_Field
                       (M, Original_Type, Canonical_Discriminant_For (Discrim),
                        True);
                     if Present (Variant_MD) then
                        Toplevel_Members.Append (Variant_MD);
                     end if;
                  end if;
                  MD := Create_Variant_Part (DI_Builder, Variant_MD, Parts);
                  Members_To_Update.Append (MD);
               end if;
            end if;
         end;
      end if;

      declare
         Members : Metadata_Array (Members_To_Update.First_Index
                                     .. Members_To_Update.Last_Index);
      begin
         for J in Members'Range loop
            Members (J) := Members_To_Update (J);
         end loop;

         return Members;
      end;

   end Convert_RI_Chain;

   ----------------------
   -- Convert_RI_Chain --
   ----------------------

   function Convert_RI_Chain (M : in out Discriminant_Map;
                              Start : Record_Info_Id;
                              Original_Type : Entity_Id;
                              Is_Union : Boolean)
     return Metadata_Array
   is
      Toplevel_Members : Member_Vector;
   begin
      return Convert_RI_Chain (M, Start, Original_Type, Is_Union,
                               Toplevel_Members, True);
   end Convert_RI_Chain;

   ------------------------------
   -- Create_Record_Debug_Info --
   ------------------------------

   function Create_Record_Debug_Info (TE : Void_Or_Type_Kind_Id;
                                      Original_Type : Entity_Id;
                                      Debug_Scope : Metadata_T;
                                      Name : String;
                                      Size : ULL;
                                      Align : Nat;
                                      S : Source_Ptr) return Metadata_T
   is
      Empty_Fields : Metadata_Array (1 .. 0);
      Result : Metadata_T;
      Is_Union : constant Boolean := Is_Unchecked_Union (Original_Type);
      Is_Unspecified : Boolean := False;
   begin
      if Original_Type /= TE
         and then Present (Get_Debug_Metadata (Original_Type))
      then
         return Get_Debug_Metadata (Original_Type);
      end if;

      --  A type might be self-referential.  For example, a record may
      --  have a member whose type refers back to the same record
      --  type.  To handle this case, we construct a empty composite
      --  type and record it; then later we update the members of the
      --  type.  Note that we pass a unique identifier here; that
      --  prevents LLVM from reusing an existing type in the case that
      --  this composite is nameless.
      if Is_Unchecked_Union (TE) then
         Result := DI_Create_Union_Type
           (Debug_Scope, Name,
            Get_Debug_File_Node (Get_Source_File_Index (S)),
            Get_Physical_Line_Number (S), Size, Align, DI_Flag_Zero,
            Empty_Fields, 0, Name);
      elsif Known_Static_RM_Size (Original_Type) then
         Result := DI_Create_Struct_Type
           (Debug_Scope, Name,
            Get_Debug_File_Node (Get_Source_File_Index (S)),
            Get_Physical_Line_Number (S), Size, Align, DI_Flag_Zero,
            No_Metadata_T, Empty_Fields, 0, No_Metadata_T, Name);
      elsif Types_Can_Have_Dynamic_Offsets then
         declare
            Expr_Size : constant Metadata_T :=
              Convert_To_Dwarf_Expression (RM_Size (Original_Type),
                                           Original_Type);
         begin
            Result := DI_Create_Struct_Type
              (Debug_Scope, Name,
               Get_Debug_File_Node (Get_Source_File_Index (S)),
               Get_Physical_Line_Number (S), Expr_Size, Align, DI_Flag_Zero,
               No_Metadata_T, Empty_Fields, 0, No_Metadata_T, Name);
         end;
      else
         --  Type with non-constant size, but the LLVM in use doesn't
         --  support that.  Just turn it into an unspecified type.
         Result := DI_Create_Unspecified_Type (Name);
         Is_Unspecified := True;
      end if;

      Set_Debug_Metadata (TE, Result);
      if Original_Type /= TE then
         Set_Debug_Metadata (Original_Type, Result);
      end if;

      --  Bail out early if we didn't actually create a record.
      if Is_Unspecified then
         return Result;
      end if;

      declare
         M : Discriminant_Map;
         Ridx : constant Record_Info_Id := Get_Record_Info (Original_Type);
         Members : constant Metadata_Array :=
           Convert_RI_Chain (M, Ridx, Original_Type, Is_Union);
      begin
         --  At least in theory it seems that LLVM may replace
         --  the object entirely, so don't assume Result will be
         --  the same, and be sure to clear it from the cache.
         Result := Replace_Composite_Elements (DI_Builder, Result, Members);

         if Original_Type /= TE then
            Clear_Debug_Metadata (Original_Type);
            Set_Debug_Metadata (Original_Type, Result);
         end if;
         Clear_Debug_Metadata (TE);

         return Result;
      end;

   end Create_Record_Debug_Info;

end GNATLLVM.Records.Debug;
