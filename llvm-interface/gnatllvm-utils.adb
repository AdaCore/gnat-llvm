------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2021, AdaCore                     --
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

with System.Storage_Elements; use System.Storage_Elements;

with LLVM.Core; use LLVM.Core;

with Errout;   use Errout;
with Output;   use Output;
with Sem_Aux;  use Sem_Aux;
with Sem_Eval; use Sem_Eval;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;

with GNATLLVM.Types;   use GNATLLVM.Types;
with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

package body GNATLLVM.Utils is

   procedure Dump_New_Line;
   --  Write an EOL to the standard output file

   Last_Struct_Id : Struct_Id := Struct_Id_Low_Bound;

   -------------------
   -- New_Struct_Id --
   -------------------

   function New_Struct_Id return Struct_Id is
   begin
      Last_Struct_Id := Last_Struct_Id + 1;
      return Last_Struct_Id;
   end New_Struct_Id;

   ------------------
   -- Hash_Value_T --
   ------------------

   function Hash_Value_T (Val : Value_T) return Hash_Type is
      function UC is new Ada.Unchecked_Conversion (Value_T, System.Address);

   begin
      return Hash_Type'Mod (To_Integer (UC (Val)) / (Val'Size / 8));
   end Hash_Value_T;

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

            if Is_Type (Entity (N)) and then not Decls_Only then
               Decode_Range (Scalar_Range (Full_Etype (N)), Low, High);
            else
               Low  := Get_Uint_Value (N);
               High := Low;
            end if;

         when N_Subtype_Indication =>
            Decode_Range (Range_Expression (Constraint (N)), Low, High);

         when N_Has_Bounds =>
            Low  := Get_Uint_Value (Low_Bound (N));
            High := Get_Uint_Value (High_Bound (N));

         when N_Character_Literal | N_Integer_Literal =>
            Low  := Get_Uint_Value (N);
            High := Low;

         when others =>
            pragma Assert (Decls_Only);
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
         return +Length_M1 + 1;
      end if;

   end Range_Length;

   -------------------
   -- Number_Bounds --
   -------------------

   function Number_Bounds (TE : Array_Kind_Id) return Nat is
      N : Opt_N_Is_Index_Id;

   begin
      if Ekind (TE) = E_String_Literal_Subtype then
         return 1;

      else
         return Num : Nat := 0 do
            N := First_Index (TE);
            while Present (N) loop
               Num := Num +
                 (if   Nkind (N) = N_Subtype_Indication
                       and then Is_Fixed_Lower_Bound_Index_Subtype (Etype (N))
                  then 1 else 2);
               Next_Index (N);
            end loop;
         end return;
      end if;
   end Number_Bounds;

   --------------------
   -- Get_Uint_Value --
   --------------------

   function Get_Uint_Value (N : N_Subexpr_Id) return Uint is
      E : Entity_Id;

   begin
      case Nkind (N) is
         when N_Integer_Literal =>
            return Intval (N);

         when N_Real_Literal =>
            --  We can only do something here if this is a fixed-point type.

            if Is_Fixed_Point_Type (Full_Etype (N)) then
               return Corresponding_Integer_Value (N);
            else
               return No_Uint;
            end if;

         when N_Has_Entity =>

            --  If there's no entity, we don't know the value unless it's a
            --  character literal.

            E := Entity (N);
            if No (E) then
               return (if   Nkind (N) = N_Character_Literal
                       then Char_Literal_Value (N) else No_Uint);
            end if;

            --  If an N_Identifier is static, its N_Defining_Identifier is
            --  either an E_Constant or an E_Enumeration_Literal.

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

   ----------------------
   -- Check_Convention --
   ----------------------

   procedure Check_Convention (E : Entity_Id) is
   begin
      if Convention (E) = Convention_CPP then
         Error_Msg_NE ("Convention C++ not supported", E, E);
      end if;
   end Check_Convention;

   ----------------------------
   -- List_Length_Non_Pragma --
   ----------------------------

   function List_Length_Non_Pragma (List : List_Id) return Nat is
      Node : Node_Id := First_Non_Pragma (List);

   begin
      return Result : Nat := 0 do
         while Present (Node) loop
            Result := Result + 1;
            Next_Non_Pragma (Node);
         end loop;
      end return;

   end List_Length_Non_Pragma;

   -----------------
   -- Num_Actuals --
   -----------------

   function Num_Actuals (N : N_Subprogram_Call_Id) return Nat is
      Actual : Node_Id := First_Actual (N);

   begin
      return Result : Nat := 0 do
         while Present (Actual) loop
            Result := Result + 1;
            Next_Actual (Actual);
         end loop;
      end return;

   end Num_Actuals;

   ---------------------
   -- Has_Full_Access --
   ---------------------

   function Has_Full_Access (N : N_Subexpr_Id) return Boolean is
      E : Entity_Id;

   begin
      case Nkind (N) is
         when N_Identifier | N_Expanded_Name =>
            E := Entity (N);
            return Is_Object (E)
              and then (Is_Full_Access (E)
                          or else Is_Full_Access (Full_Etype (E)));

         when N_Selected_Component =>
            E := Entity (Selector_Name (N));
            return Is_Full_Access (E)
              or else Is_Full_Access (Full_Etype (E));

         when N_Indexed_Component | N_Explicit_Dereference =>
            return Is_Full_Access (Full_Etype (N));

         when others =>
            return False;
      end case;
   end Has_Full_Access;

   ---------------------
   -- Get_Acting_Spec --
   ---------------------

   function Get_Acting_Spec
     (Subp_Body : N_Subprogram_Body_Id) return Node_Id
   is
   begin
      if Acts_As_Spec (Subp_Body) then
         return Specification (Subp_Body);
      else
         return Parent (Corresponding_Spec (Subp_Body));
      end if;
   end Get_Acting_Spec;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (E : Entity_Id; Suffix : String := "") return String is
      Buf : Bounded_String;

   begin
      Append (Buf, Chars (E));
      Append (Buf, Suffix);
      return +Buf;
   end Get_Name;

   ------------------
   -- Get_Ext_Name --
   ------------------

   function Get_Ext_Name
     (E : Entity_Id; Suffix : String := "") return Name_Id
   is
      Buf : Bounded_String;

   begin
      --  If we have an interface name, copy either the entire name or
      --  all but the first character of the name, depending on whether
      --  the first character is a "*".

      if E not in Formal_Kind_Id and then No (Address_Clause (E))
        and then Present (Interface_Name (E))
      then
         declare
            Str : constant String_Id := Strval (Interface_Name (E));

         begin
            if Get_Character (Get_String_Char (Str, 1)) = '*' then
               for J in 2 .. String_Length (Str) loop
                  Append (Buf, Get_Character (Get_String_Char (Str, J)));
               end loop;
            else
               Append (Buf, Str);
            end if;
         end;

      --  If we don't have a suffix, just use the name of E. Otherwise,
      --  set the name of E into the buffer.

      elsif Suffix = "" then
         return Chars (E);
      else
         Append (Buf, Chars (E));
      end if;

      --  Finally, append the suffix and make a name

      Append (Buf, Suffix);
      return Name_Find (Buf);
   end Get_Ext_Name;

   ---------------
   -- To_String --
   ---------------

   function To_String (J : Nat) return String is
      Str : constant String := Nat'Image (J);

   begin
      --  The standard guarantees that we'll have at most one blank and,
      --  since we know that the value is non-negative, exactly one blank.

      return Str (Str'First + 1 .. Str'Last);
   end To_String;

   -------------
   -- Is_Name --
   -------------

   function Is_Name (N : N_Subexpr_Id) return Boolean is
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
            elsif Count_Struct_Element_Types (T1) = 0 then
               return True;
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

   ------------------------
   -- Set_Linker_Section --
   ------------------------

   procedure Set_Linker_Section (V : GL_Value; E : Entity_Id) is
   begin
      --  Exceptions don't support linker sections

      if Ekind (E) /= E_Exception and then Present (Linker_Section_Pragma (E))
      then
         declare
            P     : constant Node_Id   := Linker_Section_Pragma (E);
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

   procedure Process_Pragmas (E : Entity_Id; V : GL_Value) is
   begin
      --  We call Get_Pragma to see if an interesting pragma is present
      --  instead of walking the loop.  This is quadratic, but the number
      --  of pragma is small and makes the code easier to read.

      if Present (Get_Pragma (E, Pragma_Weak_External)) then
         Set_Linkage (V, External_Weak_Linkage);
      end if;

   end Process_Pragmas;

   --------------------------------
   -- Enclosing_Subprogram_Scope --
   --------------------------------

   function Enclosing_Subprogram_Scope (E : Entity_Id) return Entity_Id is
   begin
      return S : Entity_Id := Scope (E) do
         loop
            exit when S = Standard_Standard
              or else Ekind (S) in E_Function | E_Procedure;

            if Is_Private_Type (S) then
               S := Full_View (S);
            else
               S := Scope (S);
            end if;
         end loop;

         if S = Standard_Standard then
            S := Empty;
         end if;
      end return;
   end Enclosing_Subprogram_Scope;

   --------------------------
   -- Set_Orig_By_Ref_Mech --
   --------------------------

   procedure Set_Orig_By_Ref_Mech (E : Formal_Kind_Id; F : Boolean) is
   begin
      Set_Flag1 (E, F);
   end Set_Orig_By_Ref_Mech;

   -------------------------
   -- Set_Added_To_Module --
   -------------------------

   procedure Set_Added_To_Module (E : Subprogram_Kind_Id; F : Boolean := True)
   is
   begin
      Set_Flag1 (E, F);
   end Set_Added_To_Module;

   ------------------------------
   -- Set_Allocated_For_Return --
   ------------------------------

   procedure Set_Allocated_For_Return
     (E : Constant_Or_Variable_Kind_Id; F : Boolean := True)
   is
   begin
      Set_Flag1 (E, F);
   end Set_Allocated_For_Return;

   -----------------------
   -- Scan_Library_Item --
   -----------------------

   procedure Scan_Library_Item (U : Node_Id) is
      N : Node_Id;

   begin
      --  Skip generic package and subprograms

      if Is_Generic_Item (U) then
         return;

      --  Scan the declarations and then the unit itself

      elsif Present (Parent (U)) then
         N := First (Declarations (Aux_Decls_Node (Parent (U))));
         while Present (N) loop
            Scan (N);
            Next (N);
         end loop;
      end if;

      Scan (U);
   end Scan_Library_Item;

   ----------------------
   -- Error_Msg_NE_Num --
   ----------------------

   procedure Error_Msg_NE_Num
     (Msg : String; N : Node_Id; E : Entity_Id; Num : Int) is
   begin
      Error_Msg_Uint_1 := +Num;
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

   -------------------
   -- Dump_New_Line --
   -------------------

   procedure Dump_New_Line is
   begin
      Push_Output;
      Set_Standard_Error;
      Write_Eol;
      Pop_Output;
   end Dump_New_Line;

   ---------------------
   -- Dump_LLVM_Value --
   ---------------------

   procedure Dump_LLVM_Value (V : Value_T) is
   begin
      Dump_Value (V);
      Dump_New_Line;
   end Dump_LLVM_Value;

   ------------------------
   -- Dump_LLVM_Metadata --
   ------------------------

   procedure Dump_LLVM_Metadata (MD : Metadata_T) is
   begin
      Dump_Metadata (MD);
      Dump_New_Line;
   end Dump_LLVM_Metadata;

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
   begin
      Dump_Type (T);
      Dump_New_Line;
   end Dump_LLVM_Type;

end GNATLLVM.Utils;
