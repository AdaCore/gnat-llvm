------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2019, AdaCore                     --
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Errout;   use Errout;
with Output;   use Output;
with Sem_Aux;  use Sem_Aux;
with Sem_Eval; use Sem_Eval;
with Snames;   use Snames;
with Sprint;   use Sprint;
with Stringt;  use Stringt;

with GNATLLVM.GLType; use GNATLLVM.GLType;
with GNATLLVM.Types;  use GNATLLVM.Types;

package body GNATLLVM.Utils is

   --------------------------
   -- Get_Current_Position --
   --------------------------

   function Get_Current_Position return Position_T is
      BB : constant Basic_Block_T := Get_Insert_Block (IR_Builder);

   begin
      return (BB, Get_Last_Instruction (BB));
   end Get_Current_Position;

   --------------------------
   -- Set_Current_Position --
   --------------------------

   procedure Set_Current_Position (P : Position_T) is
      BB   : constant Basic_Block_T := P.BB;
      Next : Value_T;

   begin
      --  There are two tricky parts here.  First is that if there's no
      --  instruction, LLVM will treat this as a request to insert at the
      --  end of a basic block, but we mean the beginning.  So we need to
      --  get the first instruction in the block and set the insertion
      --  point in front of it.  Secondly, if we have an instruction, the
      --  builder operation inserts in front of it, but we want to insert
      --  after.  The way to do that is to get the next instruction and
      --  insert before that, but there may not be another instruction.
      --  If so, then insert at the end of the block.

      if Present (P.Instr) then
         Next := Get_Next_Instruction (P.Instr);
         if Present (Next) then
            Position_Builder (IR_Builder, BB, Next);
         else
            Position_Builder_At_End (IR_Builder, BB);
         end if;
      else
         Position_Builder (IR_Builder, BB, Get_First_Instruction (BB));
      end if;
   end Set_Current_Position;

   ------------------
   -- Decode_Range --
   ------------------

   procedure Decode_Range (N : Node_Id; Low, High : out Uint) is
   begin
      case Nkind (N) is
         when N_Identifier | N_Expanded_Name =>

            --  An N_Identifier can either be a type, in which case we look
            --  at the range of the type, or a constant, in which case we
            --  look at the initializing expression.

            if Is_Type (Entity (N)) then
               Decode_Range (Scalar_Range (Full_Etype (N)), Low, High);
            else
               Low  := Get_Uint_Value (N);
               High := Low;
            end if;

         when N_Subtype_Indication =>
            Decode_Range (Range_Expression (Constraint (N)), Low, High);

         when N_Range | N_Signed_Integer_Type_Definition =>
            Low  := Get_Uint_Value (Low_Bound (N));
            High := Get_Uint_Value (High_Bound (N));

         when N_Character_Literal | N_Integer_Literal =>
            Low  := Get_Uint_Value (N);
            High := Low;

         when others =>
            Error_Msg_N ("unknown range operand", N);
            Low  := No_Uint;
            High := No_Uint;
      end case;
   end Decode_Range;

   ------------------
   -- Range_Length --
   ------------------

   function Range_Length
     (Low, High : Uint; Max_Length : Int := Int'Last) return Nat
   is
      Length_M1 : constant Uint := High - Low;

   begin
      --  We compute the range as a Uint above so we can do it in infinite
      --  precision.  We don't care whether the individual values are
      --  within the range of Int (though our caller might), only if the
      --  result is.  Since the second test is comparing Uint's, it tests
      --  both for out of range of Int and being larger than the Max_Length.

      if Length_M1 <= 0 then
         return 0;
      elsif Length_M1 > Max_Length - 1 then
         return Max_Length;
      else
         return UI_To_Int (Length_M1) + 1;
      end if;

   end Range_Length;

   --------------------
   -- Get_Uint_Value --
   --------------------

   function Get_Uint_Value (N : Node_Id) return Uint is
      E : Entity_Id;

   begin
      case Nkind (N) is
         when N_Character_Literal =>
            --  If a Entity is present, it means that this was one of the
            --  literals in a user-defined character type.

            return
              (if   Present (Entity (N)) then Enumeration_Rep (Entity (N))
               else Char_Literal_Value (N));

         when N_Integer_Literal =>
            return Intval (N);

         when N_Real_Literal =>
            --  We can only do something here if this is a fixed-point type.

            if Is_Fixed_Point_Type (Full_Etype (N)) then
               return Corresponding_Integer_Value (N);
            else
               return No_Uint;
            end if;

         when N_Identifier | N_Expanded_Name =>
            --  If an N_Identifier is static, its N_Defining_Identifier is
            --  either an E_Constant or an E_Enumeration_Literal.

            E := Entity (N);
            if Ekind (E) = E_Constant
              and then Present (Constant_Value (E))
            then
               return Get_Uint_Value (Constant_Value (E));
            elsif Ekind (E) = E_Enumeration_Literal then
               return Enumeration_Rep (E);
            else
               return No_Uint;
            end if;

         when others =>
            return No_Uint;
      end case;
   end Get_Uint_Value;

   ----------------------------
   -- List_Length_Non_Pragma --
   ----------------------------

   function List_Length_Non_Pragma (List : List_Id) return Nat is
      Node   : Node_Id;

   begin
      return  Result : Nat := 0 do
         Node := First_Non_Pragma (List);
         while Present (Node) loop
            Result := Result + 1;
            Next_Non_Pragma (Node);
         end loop;
      end return;

   end List_Length_Non_Pragma;

   -------------
   -- Discard --
   -------------

   procedure Discard (V : Value_T) is
      pragma Unreferenced (V);

   begin
      null;
   end Discard;

   -------------
   -- Discard --
   -------------

   procedure Discard (T : Type_T) is
      pragma Unreferenced (T);

   begin
      null;
   end Discard;

   -------------
   -- Discard --
   -------------

   procedure Discard (BB : Basic_Block_T) is
      pragma Unreferenced (BB);

   begin
      null;
   end Discard;

   ---------------------
   -- Get_Acting_Spec --
   ---------------------

   function Get_Acting_Spec (Subp_Body : Node_Id) return Node_Id is
   begin
      if Acts_As_Spec (Subp_Body) then
         return Specification (Subp_Body);
      else
         return Parent (Corresponding_Spec (Subp_Body));
      end if;
   end Get_Acting_Spec;

   ------------------
   -- Get_Ext_Name --
   ------------------

   function Get_Ext_Name (E : Entity_Id) return String is
      Buf : Bounded_String;

   begin
      if Present (Interface_Name (E)) and then No (Address_Clause (E)) then
         Append (Buf, Strval (Interface_Name (E)));
         return +Buf;
      else
         return Get_Name_String (Chars (E));
      end if;
   end Get_Ext_Name;

   -------------
   -- Is_Name --
   -------------

   function Is_Name (N : Node_Id) return Boolean is
   begin
      case Nkind (N) is
         when N_Identifier | N_Expanded_Name | N_Explicit_Dereference =>
            return True;

         when N_Expression_With_Actions =>
            return Is_Name (Expression (N));

         when  N_Indexed_Component | N_Slice | N_Selected_Component =>
            return Is_Name (Prefix (N));

         when others =>
            return False;
      end case;
   end Is_Name;

   -------------------------
   -- Is_Layout_Identical --
   -------------------------

   function Is_Layout_Identical (T1, T2 : Type_T) return Boolean is
      Kind1 : constant Type_Kind_T := Get_Type_Kind (T1);
      Kind2 : constant Type_Kind_T := Get_Type_Kind (T2);

   begin
      --  If the types are the same, they're identical, but if they have
      --  different kinds, they aren't.

      if T1 = T2 then
         return True;
      elsif Kind1 /= Kind2 then
         return False;
      end if;

      --  Otherwise, it's kind-specific

      case Kind1 is
         when Array_Type_Kind =>

            --  Arrays are identifical if their lengths are the same and
            --  component types are identical.

            return Get_Array_Length (T1) = Get_Array_Length (T2)
              and then Is_Layout_Identical (Get_Element_Type (T1),
                                            Get_Element_Type (T2));
         when Struct_Type_Kind =>

            --  Structures are identical if their packed status is the
            --  same, they have the same number of fields, and each field
            --  is identical.

            if Is_Packed_Struct (T1) /= Is_Packed_Struct (T2)
              or else (Count_Struct_Element_Types (T1) /=
                         Count_Struct_Element_Types (T2))
            then
               return False;
            end if;

            for J in 0 .. Count_Struct_Element_Types (T1) - 1 loop
               if not Is_Layout_Identical (Struct_Get_Type_At_Index (T1, J),
                                           Struct_Get_Type_At_Index (T2, J))
               then
                  return False;
               end if;
            end loop;

            return True;

         when others =>
            --  Otherwise, types are only identical if they're the same
            --  and that was checked above.

            return False;
      end case;
   end Is_Layout_Identical;

   ----------------------
   -- Are_In_Dead_Code --
   ----------------------

   function Are_In_Dead_Code return Boolean is
      Last_Inst : constant Value_T :=
        Get_Last_Instruction (Get_Insert_Block (IR_Builder));
   begin
      --  We're in dead code if there is an instruction in this basic block
      --  and the last is a terminator.

      return Present (Last_Inst)
        and then Present (Is_A_Terminator_Inst (Last_Inst));
   end Are_In_Dead_Code;

   -----------------------------
   -- Position_Builder_At_End --
   -----------------------------

   procedure Position_Builder_At_End (BB : Basic_Block_T) is
   begin
      Position_Builder_At_End (IR_Builder, BB);
   end Position_Builder_At_End;

   --------------
   -- Build_Br --
   --------------

   procedure Build_Br (BB : Basic_Block_T) is
   begin
      if not Are_In_Dead_Code then
         Discard (Build_Br (IR_Builder, BB));
      end if;
   end Build_Br;

   --------------------
   -- Maybe_Build_Br --
   --------------------

   procedure Maybe_Build_Br (BB : Basic_Block_T) is
   begin
      if not Are_In_Dead_Code and then Present (BB) then
         Build_Br (BB);
      end if;
   end Maybe_Build_Br;

   ----------------
   -- Move_To_BB --
   ----------------

   procedure Move_To_BB (BB : Basic_Block_T) is
   begin
      if Present (BB) then
         Build_Br (BB);
         Position_Builder_At_End (BB);
      end if;
   end Move_To_BB;

   ---------
   -- GEP --
   ---------

   function GEP
     (Bld     : Builder_T;
      Ptr     : Value_T;
      Indices : Value_Array;
      Name    : String := "") return Value_T
   is
     (GEP (Bld, Ptr, Indices'Address, Indices'Length, Name));

   ------------------------
   -- Set_Linker_Section --
   ------------------------

   procedure Set_Linker_Section (V : GL_Value; Def_Ident : Entity_Id) is
   begin
      --  Exceptions don't support linker sections

      if Ekind (Def_Ident) /= E_Exception
        and then Present (Linker_Section_Pragma (Def_Ident))
      then
         declare
            P     : constant Node_Id   := Linker_Section_Pragma (Def_Ident);
            List  : constant List_Id   := Pragma_Argument_Associations (P);
            Str   : constant Node_Id   := Expression (Last (List));
            S_Id  : constant String_Id := Strval (Expr_Value_S (Str));
            S     : String (1 .. Integer (String_Length (S_Id)));

         begin
            for J in S'Range loop
               S (J) := Get_Character (Get_String_Char (S_Id, Nat (J)));
            end loop;

            Set_Section (V, S);
         end;
      end if;
   end Set_Linker_Section;

   ---------------------
   -- Process_Pragmas --
   ---------------------

   procedure Process_Pragmas (Def_Ident : Entity_Id; V : GL_Value) is
   begin
      --  We call Get_Pragma to see if an interesting pragma is present
      --  instead of walking the loop.  This is quadratic, but the number
      --  of pragma is small and makes the code easier to read.

      if Present (Get_Pragma (Def_Ident, Pragma_Weak_External)) then
         Set_Linkage (V, External_Weak_Linkage);
      end if;

   end Process_Pragmas;

   ----------------------
   -- Error_Msg_NE_Num --
   ----------------------

   procedure Error_Msg_NE_Num
     (Msg : String; N : Node_Id; E : Entity_Id; Num : Int) is
   begin
      Error_Msg_Uint_1 := UI_From_Int (Num);
      Error_Msg_NE (Msg, N, E);
   end Error_Msg_NE_Num;

   ----------------------
   -- Error_Msg_NE_Num --
   ----------------------

   procedure Error_Msg_NE_Num
     (Msg : String; N : Node_Id; E : Entity_Id; Num : Uint) is
   begin
      Error_Msg_Uint_1 := Num;
      Error_Msg_NE (Msg, N, E);
   end Error_Msg_NE_Num;

   pragma Annotate (Xcov, Exempt_On, "Debug helpers");

   ---------------------
   -- Dump_LLVM_Value --
   ---------------------

   procedure Dump_LLVM_Value (V : Value_T) is
   begin
      Dump_Value (V);
      New_Line (Current_Error);
   end Dump_LLVM_Value;

   -------------------
   -- Dump_GL_Value --
   -------------------

   procedure Dump_GL_Value (V : GL_Value) is
   begin
      if No (V) then
         Write_Line ("None");
         return;
      end if;

      Dump_LLVM_Value (V.Value);
      Dump_LLVM_Type (Type_Of (V.Value));
      if Is_Pristine (V) then
         Write_Str ("Pristine ");
      end if;
      Write_Str (GL_Relationship'Image (V.Relationship) & "(");
      Dump_GL_Type_Int (V.Typ, False);
      Write_Str ("): ");
      pg (Union_Id (Full_Etype (V.Typ)));
   end Dump_GL_Value;

   ------------------
   -- Dump_GL_Type --
   ------------------

   procedure Dump_GL_Type (GT : GL_Type) is
   begin
      Dump_GL_Type_Int (GT, True);
   end Dump_GL_Type;

   ----------------------
   -- Dump_LLVM_Module --
   ----------------------

   procedure Dump_LLVM_Module (M : Module_T) is
   begin
      Dump_Module (M);
   end Dump_LLVM_Module;

   --------------------
   -- Dump_LLVM_Type --
   --------------------

   procedure Dump_LLVM_Type (T : Type_T) is
      procedure Dump_LLVM_Type_C (T : Type_T)
        with Import, Convention => C, External_Name => "Dump_LLVM_Type_C";

   begin
      Dump_LLVM_Type_C (T);
   end Dump_LLVM_Type;

   pragma Annotate (Xcov, Exempt_Off, "Debug helpers");

end GNATLLVM.Utils;
