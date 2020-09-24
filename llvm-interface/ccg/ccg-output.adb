------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020, AdaCore                          --
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

with Output; use Output;

with LLVM.Core; use LLVM.Core;

with CCG.Aggregates;   use CCG.Aggregates;
with CCG.Helper;       use CCG.Helper;
with CCG.Instructions; use CCG.Instructions;
with CCG.Subprograms;  use CCG.Subprograms;

package body CCG.Output is

   function Is_Simple_Type (T : Type_T) return Boolean is
     (Get_Type_Kind (T) in Half_Type_Kind .. Integer_Type_Kind
        or else Get_Type_Kind (T) = Pointer_Type_Kind)
     with Pre => Present (T);
   --  True if this is a type that's simple (elementary)

   function Is_Simple_Constant (V : Value_T) return Boolean is
     ((Get_Value_Kind (V)
         in Constant_Int_Value_Kind | Constant_Pointer_Null_Value_Kind
            | Constant_FP_Value_Kind | Constant_Expr_Value_Kind)
      or else (Is_Undef (V) and then Is_Simple_Type (Type_Of (V))))
     with Pre => Present (V);
   --  True if this is a simple enough constant that we output it in C
   --  source as a constant.
   --  ??? Strings are also simple constants, but we don't support them just
   --  yet.

   procedure Write_C_Name (S : String)
     with Pre => S'Length > 0;
   --  Write S as a valid name in C

   procedure Write_Value_Name (V : Value_T)
     with Pre => Present (V);
   --  Write the value name of V, which is either the LLVM name or a name
   --  we generate from a serial number.

   procedure Write_Constant_Value (V : Value_T)
     with Pre => Is_A_Constant (V);
   --  Write the constant value of V

   procedure Write_Undef (T : Type_T)
     with Pre => Present (T);
   --  Write an undef of type T

   procedure Maybe_Write_Comma (J : Nat) with Inline;
   --  If J is nonzero, write a comma

   procedure Write_C_Char_Code (CC : Character);
   --  Write the appropriate C code for character CC

   Hex : constant array (Integer range 0 .. 15) of Character :=
     "0123456789abcdef";

   -----------------------
   -- Maybe_Write_Comma --
   -----------------------

   procedure Maybe_Write_Comma (J : Nat) is
   begin
      if J /= 0 then
         Write_Str (", ");
      end if;
   end Maybe_Write_Comma;

   ------------------
   -- Write_C_Name --
   ------------------

   procedure Write_C_Name (S : String) is
   begin
      --  We assume here that the only characters we have to be concerned
      --  about are "." and "-", both of which we remap to "___".

      for C of S loop
         if C in '.' | '-' then
            Write_Str ("___");
         else
            Write_Char (C);
         end if;
      end loop;
   end Write_C_Name;

   ----------------------
   -- Write_Value_Name --
   ----------------------

   procedure Write_Value_Name (V : Value_T) is
   begin
     --  If it has a name, write that name and we're done.  Otherwise,
     --  mark it as not having a name if we haven't already.

      if not Get_No_Name (V) then
         declare
            S : constant String := Get_Value_Name (V);

         begin
            if S'Length > 0 then
               Write_C_Name (S);
               return;
            end if;

            Set_No_Name (V);
         end;
      end if;

      --  Print (and make if necessary) an internal name for this value

      Write_Str ("ccg_v");
      Write_Int (Get_Output_Idx (V));

   end Write_Value_Name;

   -----------------------
   -- Write_C_Char_Code --
   -----------------------

   procedure Write_C_Char_Code (CC : Character) is
   begin
      --  Remaining characters in range 0 .. 255, output with most appropriate
      --  C (escape) sequence.

      case CC is
         when ASCII.BS =>
            Write_Str ("\b");

         when ASCII.FF =>
            Write_Str ("\f");

         when ASCII.LF =>
            Write_Str ("\n");

         when ASCII.CR =>
            Write_Str ("\r");

         when ASCII.HT =>
            Write_Str ("\t");

         when ASCII.VT =>
            Write_Str ("\v");

         when ' ' .. '~' =>
            if CC in '\' | '"' | ''' then
               Write_Char ('\');
            end if;

            Write_Char (CC);

         when others =>
            Write_Str ("\x");
            Write_Char (Hex ((Character'Pos (CC) / 16) mod 16));
            Write_Char (Hex (Character'Pos (CC) mod 16));
      end case;
   end Write_C_Char_Code;

   -----------------
   -- Write_Undef --
   -----------------

   procedure Write_Undef (T : Type_T) is
   begin
      --  We can write anything for undef, so we might as well write zero

      case Get_Type_Kind (T) is
         when Half_Type_Kind | Float_Type_Kind | Double_Type_Kind
            | X86_Fp80typekind | F_P128_Type_Kind | Ppc_Fp128typekind =>
            Write_Str ("0.0");

         when Integer_Type_Kind | Pointer_Type_Kind =>
            Write_Str ("0");

         when Struct_Type_Kind =>
            Write_Str ("{");
            for J in 0 .. Nat'(Count_Struct_Element_Types (T)) - 1 loop
               Maybe_Write_Comma (J);
               Write_Undef (Struct_Get_Type_At_Index (T, J));
            end loop;

            Write_Str ("}");

         when Array_Type_Kind =>
            Write_Str ("{");
            for J in 1 .. Nat'(Get_Array_Length (T)) loop
               Maybe_Write_Comma (J);
               Write_Undef (Get_Element_Type (T));
            end loop;

            Write_Str ("}");

         when others =>
            Write_Str ("<unsupported undef type>");
      end case;
   end Write_Undef;

   --------------------------
   -- Write_Constant_Value --
   --------------------------

   procedure Write_Constant_Value (V : Value_T) is
      subtype LLI is Long_Long_Integer;
   begin
      if Is_A_Constant_Int (V) then
         declare
            Val : constant LLI := Const_Int_Get_S_Ext_Value (V);

         begin
            --  ??? Also need to emit proper U/L/LL markers

            if Val in LLI (Int'First) .. LLI (Int'Last) then
               Write_Int (Int (Val));
            else
               Write_Str (Val'Image);
            end if;
         end;

      elsif Is_A_Constant_FP (V) then

         declare
            Loses_Info : Boolean;

         begin
            --  ??? It's not clear that 'Image will always do the right thing
            --  in terms of writing the proper format for a C constant,
            --  but it's at least good enough to start with and there's no
            --  obvious other mechanism.
            --  See Cprint.Write_Real_Number_Col_Check for inspiration on what
            --  we can do.

            Write_Str
              (Double'Image (Const_Real_Get_Double (V, Loses_Info)));
         end;

      --  For a struct or array, write the values individually

      elsif Is_A_Constant_Array (V) or else Is_A_Constant_Struct (V) then
         Write_Str ("{");
         for J in 0 .. Nat'(Get_Num_Operands (V)) - 1 loop
            Maybe_Write_Comma (J);
            Write_Value (Get_Operand (V, J));
         end loop;

         Write_Str ("}");

      elsif Is_A_Constant_Data_Array (V) then

         --  We handle strings and non-strings differently

         if Is_Constant_String (V) then
            Write_Str ("""");
            for C of Get_As_String (V) loop
               Write_C_Char_Code (C);
            end loop;

            Write_Str ("""");
         else
            Write_Str ("{");
            for J in 0 .. Nat'(Get_Num_CDA_Elements (V)) - 1 loop
               Maybe_Write_Comma (J);
               Write_Constant_Value (Get_Element_As_Constant (V, J));
            end loop;

            Write_Str ("}");
         end if;

      --  If it's a constant expression, treat it as an instruction

      elsif Is_A_Constant_Expr (V) then
         Process_Instruction (V);
         Write_Str (Get_C_Value (V));

      elsif Is_A_Constant_Pointer_Null (V) then
         Write_Str ("0");

      elsif Is_Undef (V) or else Is_A_Constant_Aggregate_Zero (V) then
         Write_Undef (Type_Of (V));

      else
         Write_Str ("<unknown constant>");
      end if;
   end Write_Constant_Value;

   -----------------
   -- Write_Value --
   -----------------

   procedure Write_Value
     (V              : Value_T;
      Kind           : Value_Kind := Normal;
      For_Precedence : Precedence := Primary)
     is
      C_Value : constant Str := Get_C_Value (V);

   begin
      --  If this is a variable that we're writing normally, we need to take
      --  its address.

      if Kind = Normal and then Get_Is_Variable (V) then
         Write_Str ("&");
      end if;

      --  If we've set an expression as the value of V, write it, putting
      --  in parentheses unless we know that it's of higher precedence

      if Present (C_Value) then
         if For_Precedence /= Unknown and then Has_Precedence (C_Value)
           and then Get_Precedence (C_Value) > For_Precedence
         then
            Write_Str (C_Value);
         else
            Write_Str ("(" & C_Value & ")");
         end if;

      --  If this is either a simple constant or any constant for an
      --  initializer, write the constant.

      elsif Is_Simple_Constant (V)
        or else (Kind = Initializer and then Is_A_Constant (V))
      then
         Write_Constant_Value (V);

      --  Otherwise, write the name

      else
         Write_Value_Name (V);
      end if;

   end Write_Value;

   ----------------
   -- Maybe_Decl --
   ----------------

   procedure Maybe_Decl (V : Value_T; For_Initializer : Boolean := False) is
   begin
      --  If we already processed this, we're done

      if Get_Is_Decl_Output (V) then
         return;

      --  If it's a simple constant (actual constant if this is for an
      --  initializer), we just mark us as having processed it.

      elsif Is_Simple_Constant (V)
        or else (For_Initializer and then Is_A_Constant (V))
      then
         Set_Is_Decl_Output (V);

      --  Otherwise, write the decl (which will mark it as done)

      else
         Write_Decl (V);
      end if;

   end Maybe_Decl;

   ----------------
   -- Write_Decl --
   ----------------

   procedure Write_Decl (V : Value_T) is
      Global : constant Boolean := Is_A_Global_Variable (V);

   begin
      --  We need to write a declaration for this if it's not a simple
      --  constant or constant expression, not a function, an argument, a
      --  basic block or undef, and we haven't already written one or
      --  assigned a value to it.

      if not Get_Is_Decl_Output (V) and then not Is_Simple_Constant (V)
        and then not Is_A_Function (V) and then not Is_A_Argument (V)
        and then not Is_A_Basic_Block (V) and then not Is_Undef (V)
        and then not Is_A_Constant_Expr (V) and then No (Get_C_Value (V))
      then
         Set_Is_Decl_Output (V);

         --  If this is a global, mark it as a variable

         if Global then
            Set_Is_Variable (V);
         end if;

         --  The relevant type is the type of V unless V is a
         --  variable, in which case the type of V is a pointer and we
         --  want what it points to.

         declare
            Typ  : constant Type_T :=
              (if   Get_Is_Variable (V) then Get_Element_Type (Type_Of (V))
               else Type_Of (V));
            Decl : Str             := Typ & " " & (V + Value_Name);

         begin
            --  For globals, we write the decl immediately. Otherwise,
            --  it's part of the decls for the subprogram.  Figure out
            --  whether this is static or extern.  It's extern if there's
            --  no initializer.

            if Global then
               declare
                  Init : constant Value_T := Get_Initializer (V);

               begin
                  if No (Init) then
                     Decl := "extern " & Decl;
                  elsif Get_Linkage (V) = Internal_Linkage then
                     Decl := "static " & Decl;
                  end if;

                  --  Don't write an initializer if it's undef or a
                  --  zeroinitializer. In the latter case, it means to apply
                  --  the default initialization, which is defined by the
                  --  C standard as being all zeros (hence the name).

                  if Present (Init) and then not Is_Undef (Init)
                    and then not Is_A_Constant_Aggregate_Zero (Init)
                  then
                     Decl := Decl & " = " & (Init + Initializer);
                     Maybe_Decl (Init, For_Initializer => True);
                  end if;

                  Write_Str (Decl & ";", Eol => True);
               end;
            else
               --  If this is a constant (we know that it can't be a simple
               --  constant), we need to initialize the value to that of the
               --  constant.

               if Is_A_Constant (V) then
                  Decl := Decl & " = " & (V + Initializer);
               end if;

               Output_Decl (Decl);
            end if;
         end;
      end if;

      Set_Is_Decl_Output (V);
   end Write_Decl;

   -----------------
   -- Write_Type --
   -----------------

   procedure Write_Type (T : Type_T) is
   begin
      case Get_Type_Kind (T) is

         when Void_Type_Kind =>
            Write_Str ("void");

         --  ??? For FP types, we'd ideally want to compare the number of bits
         --  and use that, but there's no simple way to do that.  So let's
         --  start with just "float" and "double".

         when Float_Type_Kind =>
            Write_Str ("float");

         when Double_Type_Kind =>
            Write_Str ("double");

         when Integer_Type_Kind =>
            declare
               Bits : constant Pos := Pos (Get_Int_Type_Width (T));

            begin
               --  ??? There are a number of issues here: Ada supports a
               --  "long long long" type, which could correspond to C's
               --  int128_t.  We also may want to generate intXX_t types
               --  instead of the standard types based on a switch.  But for
               --  now we'll keep it simple.

               if Bits > Long_Size and then Bits > Int_Size
                 and then Bits <= Long_Long_Size
               then
                  Write_Str ("long long");
               elsif Bits > Int_Size and then Bits <= Long_Size then
                  Write_Str ("long");
               elsif Bits > Short_Size and then Bits <= Int_Size then
                  Write_Str ("int");
               elsif Bits > Char_Size and then Bits <= Short_Size then
                  Write_Str ("short");
               elsif Bits <= Char_Size then
                  Write_Str ("char");
               else
                  Write_Str ("<unknown int type:" & Bits'Image & ">");
               end if;
            end;

         when Pointer_Type_Kind =>
            Write_Str (Get_Element_Type (T) & " *");

         when Struct_Type_Kind =>
            if Has_Name (T) then
               Write_C_Name (Get_Struct_Name (T));
            else
               Write_Str ("ccg_s");
               Write_Int (Get_Output_Idx (T));
            end if;

         when Array_Type_Kind =>
            Write_Str ("ccg_a");
            Write_Int (Get_Output_Idx (T));

         when others =>
            Write_Str ("<unsupported type>");
      end case;

   end Write_Type;

   -------------------
   -- Write_Typedef --
   --------------------

   procedure Write_Typedef (T : Type_T) is
   begin
      Set_Is_Typedef_Output (T);
      if Get_Type_Kind (T) = Struct_Type_Kind then
         Write_Struct_Typedef (T);
      elsif Get_Type_Kind (T) = Array_Type_Kind then
         Write_Array_Typedef (T);
      end if;

   end Write_Typedef;

   --------------
   -- Write_BB --
   --------------

   procedure Write_BB (BB : Basic_Block_T) is
   begin
     --  If it has a name, write that name and we're done.  Otherwise,
     --  mark it as not having a name if we haven't already.

      if not Get_No_Name (BB) then
         declare
            S : constant String := Get_Value_Name (Basic_Block_As_Value (BB));

         begin
            if S'Length > 0 then
               Write_C_Name (S);
               return;
            end if;

            Set_No_Name (BB);
         end;
      end if;

      Write_Str ("ccg_l");
      Write_Int (Get_Output_Idx (BB));
   end Write_BB;

end CCG.Output;
