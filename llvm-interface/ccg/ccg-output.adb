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

with Get_Targ; use Get_Targ;

with Output; use Output;

with LLVM.Core; use LLVM.Core;

with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

with CCG.Aggregates;   use CCG.Aggregates;
with CCG.Helper;       use CCG.Helper;
with CCG.Instructions; use CCG.Instructions;
with CCG.Subprograms;  use CCG.Subprograms;
with CCG.Utils;        use CCG.Utils;

package body CCG.Output is

   procedure Maybe_Write_Typedef_And_Decl (V : Value_T)
     with Pre => Is_A_Constant (V);
   --  Ensure that we're written typedefs for any types within V and
   --  declarations for anything in V that needs it

   procedure Write_Value_Name (V : Value_T)
     with Pre => Present (V);
   --  Write the value name of V, which is either the LLVM name or a name
   --  we generate from a serial number.

   procedure Write_Str_With_Precedence (S : Str; P : Precedence)
     with Pre => Present (S);
   --  Write S, but add parentheses unless we know that it's of higher
   --  precedence than P.

   procedure Write_Constant_Value
     (V             : Value_T;
      Flags         : Value_Flags := Default_Flags;
      Need_Unsigned : Boolean     := False)
     with Pre => Is_A_Constant (V);
   --  Write the constant value of V, optionally specifying a preference of
   --  the expression that it's part of.

   procedure Write_Undef (T : Type_T)
     with Pre => Present (T);
   --  Write an undef of type T

   procedure Maybe_Write_Comma (First : in out Boolean)
     with Post => not First, Inline;
   --  If First isn't set, write a comma.  Then clear First

   procedure Write_C_Char_Code (CC : Character);
   --  Write the appropriate C code for character CC

   Octal : constant array (Integer range 0 .. 7) of Character := "01234567";

   -----------------------
   -- Maybe_Write_Comma --
   -----------------------

   procedure Maybe_Write_Comma (First : in out Boolean) is
   begin
      if not First then
         Write_Str (", ");
      end if;

      First := False;
   end Maybe_Write_Comma;

   ------------------
   -- Write_C_Name --
   ------------------

   procedure Write_C_Name (S : String) is
      Append_Suffix : Boolean := False;
   begin
      --  First check for C predefined types and keywords. Note that we do not
      --  need to check C keywords which are also Ada reserved words since
      --  (if present) they were rejected by the Ada front end.
      --  Those keywords are: case do else for goto if return while.
      --  In this case, append an _ at the end of the name.

      if S = "auto"
        or else S = "bool"
        or else S = "break"
        or else S = "char"
        or else S = "const"
        or else S = "continue"
        or else S = "default"
        or else S = "double"
        or else S = "enum"
        or else S = "extern"
        or else S = "float"
        or else S = "int"
        or else S = "long"
        or else S = "register"
        or else S = "short"
        or else S = "signed"
        or else S = "sizeof"
        or else S = "static"
        or else S = "struct"
        or else S = "switch"
        or else S = "typedef"
        or else S = "union"
        or else S = "unsigned"
        or else S = "void"
        or else S = "volatile"
      then
         Write_Str (S);
         Append_Suffix := True;
      else
         --  We assume here that the only characters we have to be concerned
         --  about are "." and "-", which we remap to "_".
         --  GNAT LLVM itself only generates '.' but the LLVM optimizer may
         --  generate e.g. .pre-phixxx variables, see
         --  lib/Transforms/Scalar/GVN.cpp or xxxthread-pre-split, see
         --  lib/Transforms/Scalar/JumpThreading.cpp.

         for C of S loop
            if C in '.' | '-' then
               Append_Suffix := True;
               Write_Char ('_');
            else
               Write_Char (C);
            end if;
         end loop;
      end if;

      --  If needed, append also an "_" to make a name unique wrt Ada
      --  identifiers.

      if Append_Suffix then
         Write_Char ('_');
      end if;
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
            Write_Char ('\');
            Write_Char (Octal ((Character'Pos (CC) / 64)));
            Write_Char (Octal ((Character'Pos (CC) / 8) mod 8));
            Write_Char (Octal (Character'Pos (CC) mod 8));
      end case;
   end Write_C_Char_Code;

   -----------------
   -- Write_Undef --
   -----------------

   procedure Write_Undef (T : Type_T) is
      First : Boolean := True;

   begin
      --  We can write anything for undef, so we might as well write zero

      case Get_Type_Kind (T) is
         when Half_Type_Kind | Float_Type_Kind | Double_Type_Kind
            | X86_Fp80typekind | F_P128_Type_Kind | Ppc_Fp128typekind =>
            Write_Str ("0.0");

         when Integer_Type_Kind =>
            Write_Str ("0");

         when Pointer_Type_Kind =>
            Write_Str ("NULL");

         when Struct_Type_Kind =>
            Write_Str ("{");
            for J in 0 .. Nat'(Count_Struct_Element_Types (T)) - 1 loop
               Maybe_Write_Comma (First);
               Write_Undef (Struct_Get_Type_At_Index (T, J));
            end loop;

            --  For an empty struct, write out a value for the dummy field

            if Count_Struct_Element_Types (T) = 0 then
               Write_Str ("0");
            end if;

            Write_Str ("}");

         when Array_Type_Kind =>
            Write_Str ("{");
            for J in 0 .. Effective_Array_Length (T) - 1 loop
               Maybe_Write_Comma (First);
               Write_Undef (Get_Element_Type (T));
            end loop;

            Write_Str ("}");

         when others =>
            Error_Msg ("unsupported undef type: " & Get_Type_Kind (T)'Image);
            Write_Str ("<unsupported undef type: " & Get_Type_Kind (T)'Image
                       & ">");
      end case;
   end Write_Undef;

   --------------------------
   -- Write_Constant_Value --
   --------------------------

   procedure Write_Constant_Value
     (V             : Value_T;
      Flags         : Value_Flags := Default_Flags;
      Need_Unsigned : Boolean     := False)
   is
      procedure Write_Int_Qualifier (Width : Int);
      --  Write the relevant L/LL signed int qualifier

      -------------------------
      -- Write_Int_Qualifier --
      -------------------------

      procedure Write_Int_Qualifier (Width : Int) is
      begin
         if Width = Get_Long_Long_Size then
            Write_Str ("LL");
         elsif Width > Get_Int_Size then
            Write_Str ("L");
         end if;
      end Write_Int_Qualifier;

      First : Boolean := True;

   begin
      if Is_A_Constant_Int (V) then
         declare
            Width : constant Int := Get_Int_Type_Width (V);
         begin
            if Width = 1 then
               Write_Str
                 (if Const_Int_Get_S_Ext_Value (V) = 0 then "0" else "1");
            elsif Need_Unsigned then
               declare
                  U_Img : constant String :=
                    Const_Int_Get_Z_Ext_Value (V)'Image;
               begin
                  Write_Str (U_Img (2 .. U_Img'Last));
                  Write_Str ("U");
               end;
            else
               declare
                  Value : constant Long_Long_Integer :=
                    Const_Int_Get_S_Ext_Value (V);
               begin
                  --  Special case MIN_INT which cannot be expressed directly
                  --  without causing an overflow.

                  if Value = -2 ** Natural (Width - 1) then
                     Write_Str ("(" & Long_Long_Integer'Image (Value + 1));
                     Write_Int_Qualifier (Width);
                     Write_Str ("-1");
                     Write_Int_Qualifier (Width);
                     Write_Str (")");
                     return;

                  else
                     declare
                        Img : constant String := Value'Image;
                     begin
                        Write_Str
                          (Img ((if Img (1) = '-' then 1 else 2) .. Img'Last));
                     end;
                  end if;
               end;
            end if;

            Write_Int_Qualifier (Width);
         end;

      elsif Is_A_Constant_FP (V) then
         declare
            Buffer : String (1 .. 128);
            Len    : Natural;
         begin
            Len := Convert_FP_To_String (V, Buffer);
            Write_Str (Buffer (1 .. Len));
         end;

      --  For a struct or array, write the values individually

      elsif Is_A_Constant_Array (V) or else Is_A_Constant_Struct (V) then
         Write_Str ("{");
         for J in 0 .. Nat'(Get_Num_Operands (V)) - 1 loop

            --  If this is a zero-length array in a struct, omit the
            --  initializer, since we don't have that field.

            if not Is_A_Constant_Struct (V)
              or else not Is_Zero_Length_Array (Struct_Get_Type_At_Index
                                                  (Type_Of (V), J))
            then
               Maybe_Write_Comma (First);
               Maybe_Decl (Get_Operand (V, J), For_Initializer => True);
               Write_Value (Get_Operand (V, J), Flags => Flags);
            end if;
         end loop;

         --  If this is a zero-length array or struct, add an extra item

         if Nat'(Get_Num_Operands (V)) = 0 then
            Write_Undef (Get_Element_Type (V));
         end if;

         Write_Str ("}");

      elsif Is_A_Constant_Data_Array (V) then

         --  We handle strings and non-strings differently

         if Is_C_String (V) then
            Write_Str ("""");
            for C of Get_As_String (V) loop
               Write_C_Char_Code (C);
            end loop;

            Write_Str ("""");
         else
            Write_Str ("{");
            for J in 0 .. Nat'(Get_Num_CDA_Elements (V)) - 1 loop
               Maybe_Write_Comma (First);
               Write_Constant_Value (Get_Element_As_Constant (V, J));
            end loop;

            if Nat'(Get_Num_CDA_Elements (V)) = 0 then
               Write_Undef (Get_Element_Type (V));
            end if;

            Write_Str ("}");
         end if;

      elsif Is_A_Constant_Pointer_Null (V) then
         Write_Str ("NULL");

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
      Flags          : Value_Flags := Default_Flags;
      For_Precedence : Precedence  := Primary)
   is
      --  We're either writing V alone or V prefixed by one or more unary
      --  operations, such as casts and ampersand. In the former case,
      --  we may have to parenthesize V depending the relationship between
      --  its precedence and For_Precedence. In the latter case, we may need
      --  to do that and/or parenthesize our entire expression. Anything we
      --  add will have precedence Unary.

      procedure Maybe_Write_Parens;
      --  We call this when we're going to be prefixing V with some unary
      --  operation. If that means we need to parenthesize the entire
      --  expression, set that up. Also mark how V will be used.

      function Must_Write_Cast return Boolean is
         (Get_Type_Kind (V) = Integer_Type_Kind
            and then Nat (Get_Scalar_Bit_Size (Type_Of (V))) < Get_Int_Size
            and then Is_A_Instruction (V)
            and then Get_Opcode (V) not in Op_Alloca | Op_Load |
                                           Op_Extract_Value | Op_Insert_Value);
      --  Because of C's integer promotion rules, we must insert a cast if
      --  V is an integer narrower than int and the output of an
      --  arithmetic, shift, or logical instruction.

      C_Value     : constant Str := Get_C_Value (V);
      Inner_For_P : Precedence   := For_Precedence;
      Wrote_Paren : Boolean      := False;

      ------------------------
      -- Maybe_Write_Parens --
      ------------------------

      procedure Maybe_Write_Parens is
      begin
         --  Always mark the context in which V will be used as a unary
         --  expression. If we've already decided we need to write parens
         --  or if an expression of precedence Unary doesn't need parens in
         --  our context, that's all we have to do. Otherwise, write an open
         --  paren and show that we've done so.

         Inner_For_P := Unary;
         if not Wrote_Paren and then Needs_Parens (Unary, For_Precedence) then
            Write_Str ("(");
            Wrote_Paren := True;
         end if;
      end Maybe_Write_Parens;

   begin
      --  If we're to write the type of V instead of the value of V, do so

      if Flags.Write_Type then
         if Get_Is_Unsigned (V) then
            Write_Str ("unsigned ");
         end if;

         Write_Type (Type_Of (V));
         return;
      end if;

      --  Otherwise, see if we want an unsigned version of V (unless this
      --  is a pointer, which is always treated as unsigned).

      if Flags.Need_Unsigned and then not Is_Pointer_Type (V) then

         --  If this is an undef, all we need to do is write a zero because
         --  that's both signed and unsigned.

         if Is_Undef (V) then
            Write_Str ("0");
            return;

         --  If its a constant, we can write the unsigned version of that
         --  constant.

         elsif Is_A_Constant_Int (V) then
            Write_Constant_Value (V, Need_Unsigned => True);
            return;

         --  If it's not declared to be unsigned or we need to be concerned
         --  about integer promotion, write a cast and then the value. Note
         --  that we can't use #T or Write_Type here because we often want
         --  to write a different signedness than V. Likewise below.

         elsif not (Get_Is_Unsigned (V) and then Get_Is_Decl_Output (V))
           or else Must_Write_Cast
         then
            Maybe_Write_Parens;
            Write_Str ("(unsigned " & Type_Of (V) & ") ");
         end if;

         --  Otherwise, if this is an object that must be interpreted as
         --  signed but might be unsigned or we need to be concerned about
         --  integer promotion, write a cast to the signed type.

      elsif Flags.Need_Signed
        and then (Might_Be_Unsigned (V) or else Must_Write_Cast)
      then
         Write_Str ("(" & Type_Of (V) & ") ");
      end if;

      --  If this is an LHS that we're writing normally, we need to take
      --  its address. However, in C the name of an array is its address,
      --  so we can omit it in that case.

      if not Flags.LHS and then Get_Is_LHS (V) then

         --  If this is a constant, we need to convert the address into a
         --  non-constant pointer type. Likewise if it's declared as unsigned,
         --  except that in that case, the relevant aspect of the type is
         --  that it's not unsigned.

         if Get_Is_Constant (V) or else Get_Is_Unsigned (V) then
            Maybe_Write_Parens;
            Write_Str ("(" & Type_Of (V) & ") ");
         end if;

         if Get_Type_Kind (V) /= Array_Type_Kind then
            Maybe_Write_Parens;
            Write_Str ("&");
         end if;
      end if;

      --  If we've set an expression as the value of V, write it

      if Present (C_Value) and then not Flags.Phi_Temp then
         Write_Str_With_Precedence (C_Value, Inner_For_P);

      --  If this is either a simple constant or any constant for an
      --  initializer, write the constant. If this is an LHS, it means
      --  that we have its address and so we want to write the name.

      elsif not Get_Is_LHS (V)
        and then (Is_Simple_Constant (V)
                    or else (Flags.Initializer and then Is_A_Constant (V)))
      then
         Write_Constant_Value (V, Flags => Flags);

      --  Otherwise, write the name and possibly the suffix for a Phi
      --  temporary.

      else
         Write_Value_Name (V);
         if Flags.Phi_Temp then
            Write_Str ("_t_");
         end if;
      end if;

      --  If we wrote an open paren before this expression, we need to close it

      if Wrote_Paren then
         Write_Str (")");
      end if;
   end Write_Value;

   -------------------------------
   -- Write_Str_With_Precedence --
   -------------------------------

   procedure Write_Str_With_Precedence (S : Str; P : Precedence) is
   begin
      --  If S is just a single value and that value has a saved expression,
      --  write that one. This preserves precedence.

      if Is_Value (S) and then Present (Get_C_Value (Single_Value (S))) then
         Write_Str_With_Precedence (Get_C_Value (Single_Value (S)), P);
      elsif Needs_Parens (S, P) then
         Write_Str ("(" & S & ")");
      else
         Write_Str (S);
      end if;
   end Write_Str_With_Precedence;

   ----------------
   -- Maybe_Decl --
   ----------------

   procedure Maybe_Decl (V : Value_T; For_Initializer : Boolean := False) is
   begin
      --  Be sure that we've written a typedef for V's type

      Maybe_Write_Typedef (Type_Of (V));

      --  If this is an unprocessed constant expression, process it as an
      --  instruction

      if Is_A_Constant_Expr (V) and then No (Get_C_Value (V)) then
         Process_Instruction (V);
      end if;

      --  If we've already processed this or if it's a simple constant
      --  (any constant if this is for an initializer), we don't need
      --  to do anything.

      if Get_Is_Decl_Output (V)
        or else Is_Simple_Constant (V)
        or else (For_Initializer and then Is_A_Constant (V))
      then
         return;
      end if;

      --  We need to write a declaration for this if we haven't already
      --  assigned a value to it, it's not a function, an argument, a basic
      --  block or simple undef.

      if No (Get_C_Value (V)) and then not Is_A_Function (V)
        and then not Is_A_Argument (V) and then not Is_A_Basic_Block (V)
        and then not (Is_Undef (V) and then Is_Simple_Type (V))
      then
         Set_Is_Decl_Output (V);

         --  If this is a global, mark it as an LHS

         if Is_A_Global_Variable (V) then
            Set_Is_LHS (V);
         end if;

         --  The relevant type is the type of V unless V is an LHS, in
         --  which case the type of V is a pointer and we want what it
         --  points to.

         declare
            Typ  : constant Type_T :=
              (if Get_Is_LHS (V) then Get_Element_Type (V) else Type_Of (V));
            Decl : Str             := Typ & " " & (V + LHS);

         begin
            --  If this is known to be unsigned, indicate that

            if Get_Is_Unsigned (V) then
               Decl := "unsigned " & Decl;
            end if;

            --  For globals, we write the decl immediately. Otherwise, it's
            --  part of the decls for the subprogram.  Figure out whether
            --  this is static or extern.  It's extern if there's no
            --  initializer.

            if Is_A_Global_Variable (V) then
               declare
                  Init : constant Value_T := Get_Initializer (V);

               begin
                  if No (Init) then
                     Decl := "extern " & Decl;
                  elsif Get_Linkage (V) in Internal_Linkage | Private_Linkage
                  then
                     Decl := "static " & Decl;
                  end if;

                  --  If this is a constant, denote that

                  if Is_Global_Constant (V) then
                     Decl := "const " & Decl;
                     Set_Is_Constant (V);
                  end if;

                  --  Don't write an initializer if it's undef or a
                  --  zeroinitializer. In the latter case, it means to apply
                  --  the default initialization, which is defined by the
                  --  C standard as being all zeros (hence the name).

                  if Present (Init) and then not Is_Undef (Init)
                    and then not Is_A_Constant_Aggregate_Zero (Init)
                  then
                     Maybe_Write_Typedef_And_Decl (Init);
                     Decl := Decl & " = " & (Init + Initializer);
                  end if;

                  Write_Str (Decl & ";", Eol => True);
               end;
            else
               --  If this is a constant (we know that it can't be a simple
               --  constant), we need to initialize the value to that of the
               --  constant and put it at the top level.

               if Is_A_Constant (V) then
                  Maybe_Write_Typedef_And_Decl (V);
                  Write_Str ("static const " & Decl & " = " &
                               (V + Initializer) & ";", Eol => True);
                  Set_Is_Constant (V);
               else
                  Output_Decl (Decl);
               end if;
            end if;
         end;
      end if;
   end Maybe_Decl;

   ----------------------------------
   -- Maybe_Write_Typedef_And_Decl --
   ----------------------------------

   procedure Maybe_Write_Typedef_And_Decl (V : Value_T) is
   begin
      --  First, ensure that V has been declared

      Maybe_Decl (V, For_Initializer => True);

      --  Next, we'll take care of typedefs for V's type and for types
      --  recursively within it, but we also have to be concerned about
      --  constant expressions, which may reference objects that could be
      --  of types for which we haven't yet written a typedef.

      Maybe_Write_Typedef (Type_Of (V));
      if Is_A_Constant_Array (V) or else Is_A_Constant_Struct (V)
        or else Is_A_Constant_Expr (V)
      then
         for J in 0 .. Nat'(Get_Num_Operands (V)) - 1 loop
            Maybe_Write_Typedef_And_Decl (Get_Operand (V, J));
         end loop;

      elsif Is_A_Constant_Data_Array (V) then
         for J in 0 .. Nat'(Get_Num_CDA_Elements (V)) - 1 loop
            Maybe_Write_Typedef_And_Decl (Get_Element_As_Constant (V, J));
         end loop;
      end if;
   end Maybe_Write_Typedef_And_Decl;

   ----------------
   -- Int_String --
   ----------------

   function Int_String (Size : Pos) return String is
   begin
      --  ??? There are a number of issues here: Ada supports a
      --  "long long long" type, which could correspond to C's
      --  int128_t.  We also may want to generate intXX_t types
      --  instead of the standard types based on a switch.  But for
      --  now we'll keep it simple.

      if Size > Long_Size and then Size > Int_Size
        and then Size <= Long_Long_Size
      then
         return "long long";
      elsif Size > Int_Size and then Size <= Long_Size then
         return "long";
      elsif Size > Short_Size and then Size <= Int_Size then
         return "int";
      elsif Size > Char_Size and then Size <= Short_Size then
         return "short";
      elsif Size <= Char_Size then
         return "char";
      else
         return "<unknown int type:" & Size'Image & ">";
      end if;
   end Int_String;

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
            Write_Str (Int_String (Pos (Get_Int_Type_Width (T))));

         when Pointer_Type_Kind =>

            --  There's no such thing in C as a function type, only a
            --  pointer to function type. So we special-case that.

            if Get_Type_Kind (Get_Element_Type (T)) = Function_Type_Kind then
               Write_Str ("ccg_f");
               Write_Int (Get_Output_Idx (T));

            --  An array of zero size represents a variable-sized array,
            --  which C doesn't support, so consider this a pointer to the
            --  element type of the array.

            elsif Get_Type_Kind (Get_Element_Type (T)) = Array_Type_Kind
              and then Get_Array_Length (Get_Element_Type (T)) = 0
            then
               Write_Type (Get_Element_Type (Get_Element_Type (T)));
               Write_Str (" *");

            --  Otherwise, this is handled normally. We don't want to use a
            --  concatenation operator because that might cause us to try
            --  to write out the typedef for the pointed-to type, which
            --  will then be at the wrong place.

            else
               Write_Type (Get_Element_Type (T));
               Write_Str (" *");
            end if;

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
            Error_Msg ("unsupported type: " & Get_Type_Kind (T)'Image);
            Write_Str ("<unsupported type: " & Get_Type_Kind (T)'Image & ">");
      end case;
   end Write_Type;

   -------------------
   -- Write_Typedef --
   --------------------

   procedure Write_Typedef (T : Type_T; Incomplete : Boolean := False) is
   begin
      --  Show we're writing the typedef (so we know not to do it
      --  recursively).

      Set_Are_Writing_Typedef (T);

      --  See what type of type this is

      if Get_Type_Kind (T) = Struct_Type_Kind then
         Write_Struct_Typedef (T, Incomplete => Incomplete);
      elsif Get_Type_Kind (T) = Array_Type_Kind then
         Write_Array_Typedef (T);
      elsif Get_Type_Kind (T) = Pointer_Type_Kind then

         --  We don't have typedefs for function types, just pointer to
         --  function types. But for normal pointer types, make sure we've
         --  written at least an incomplete version of the typedef for the
         --  pointed-to type.

         if Get_Type_Kind (Get_Element_Type (T)) = Function_Type_Kind then
            Write_Function_Type_Typedef (T);
         else
            Maybe_Write_Typedef (Get_Element_Type (T), Incomplete => True);
         end if;
      end if;

      --  Show we've written the typedef unless this is a struct type and
      --  we're only writing an incomplete definition.

      Set_Are_Writing_Typedef (T, False);
      if not Incomplete or else Get_Type_Kind (T) /= Struct_Type_Kind then
         Set_Is_Typedef_Output   (T);
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
