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

with LLVM.Core; use LLVM.Core;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Get_Targ; use Get_Targ;

with Atree;       use Atree;
with Csets;       use Csets;
with Debug;       use Debug;
with Einfo.Utils; use Einfo.Utils;
with Opt;         use Opt;
with Osint;       use Osint;
with Osint.C;     use Osint.C;
with Output;      use Output;
with Set_Targ;    use Set_Targ;
with Sinput;      use Sinput;
with Uintp;       use Uintp;

with GNATLLVM.Codegen; use GNATLLVM.Codegen;
with GNATLLVM.Types;   use GNATLLVM.Types;
with GNATLLVM.Utils;   use GNATLLVM.Utils;
with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

with CCG.Aggregates;  use CCG.Aggregates;
with CCG.Codegen;     use CCG.Codegen;
with CCG.Environment; use CCG.Environment;
with CCG.Helper;      use CCG.Helper;
with CCG.Target;      use CCG.Target;
with CCG.Utils;       use CCG.Utils;

package body CCG.Write is

   --  To keep all related information in the same place, we define functions
   --  here that, given a block style, tell us how to write that block.

   function Need_Brace
     (BS : Block_Style; Single_Line : Boolean) return Boolean
   is
     (not Single_Line or else BS in Decl | Switch or else Always_Brace);
   --  Indicates whether we need a brace for this block. Single_Line says
   --  whether or not the block consists of just a single line.

   function Brace_Indent (BS : Block_Style) return Integer is
     (if BS = Decl then 0 else C_Indent);
   --  Gives the number of spaces to indent the brace line of a block

   function End_Brace_Needs_Semicolon (BS : Block_Style) return Boolean is
     (BS = Decl);
   --  True if this style block's closing brace needs to be followed by a
   --  semicolon.

   procedure Write_Start_Block
     (BS : Block_Style; Single_Line : Boolean; S : Str := No_Str);
   procedure Write_End_Block
     (BS : Block_Style; Single_Line : Boolean; S : Str := No_Str);
   --  Write the start or end of a C block with the specifed style.
   --  Single_Line is true iff the block only has a single line. If Present,
   --  S is the string to write, if any. It must begin with an open brace
   --  for the start of the block and an end brace for the end of the block.

   procedure Write_Internal_Name (S : String; Qualify : Boolean := True);
   --  If Qualify is true, prepend the filename to S and and write both.
   --  Otherwise, just write S

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
      Flags         : Value_Flags := Default_Value_Flags;
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

   function  Is_Comment_Line (L : Physical_Line_Number) return Boolean;
   --  Indicate whether physical line L is an Ada comment line

   procedure Write_Source_Line (L : Physical_Line_Number);
   --  Write the Ada source line L from the main file

   Octal : constant array (Integer range 0 .. 7) of Character := "01234567";

   Main_Source_Name       : Str;
   --  If -gnatL is specifed, the fully-qualified filename of the main unit

   Src                    : Source_Buffer_Ptr;
   --  If -gnatL is specified, the text of the main unit's source file

   Indent                 : Integer                    := 0;
   --  The current indentation level for all but non-indented lines

   Next_Line_To_Dump      : Physical_Line_Number       :=
     Physical_Line_Number'First;
   --  The next source line to dump

   Previous_Debug_File    : Str                        := No_Str;
   Previous_Debug_Line    : Physical_Line_Number;
   --  The filename and line number of the last #line directive we wrote,
   --  if any.

   Previous_Was_End_Block : Boolean                    := False;
   --  True if the last line written was the end of a block

   Defined_Name           : Str;
   --  Name used in #define for this .h file.

   -----------------------
   -- Write_Start_Block --
   -----------------------

   procedure Write_Start_Block
     (BS : Block_Style; Single_Line : Boolean; S : Str := No_Str)
   is
   begin
      if Present (BS) then
         if Need_Brace (BS, Single_Line) then
            Indent := Indent + Brace_Indent (BS);
            Write_C_Line ((if Present (S) then S else +"{"));
         end if;

         Indent := Indent + C_Indent;
      end if;
   end Write_Start_Block;

   ---------------------
   -- Write_End_Block --
   ---------------------

   procedure Write_End_Block
     (BS : Block_Style; Single_Line : Boolean; S : Str := No_Str)
   is
   begin
      if Present (BS) then
         if Need_Brace (BS, Single_Line) then
            Indent := Indent - C_Indent;
            Write_C_Line
              ((if    Present (S) then S
                elsif End_Brace_Needs_Semicolon (BS) then +"};" else +"}"));
         end if;

         Indent := Indent - Brace_Indent (BS);
      end if;
   end Write_End_Block;

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

   -------------------------
   -- Write_Internal_Name --
   -------------------------

   procedure Write_Internal_Name (S : String; Qualify : Boolean := True) is
   begin
      if Qualify then
         Set_File_Name ("");
         Write_C_Name (Name_Buffer (1 .. Name_Len - 1),
                       Need_Suffix => True);
      end if;

      Write_Str (S);
   end Write_Internal_Name;

   ------------------
   -- Write_C_Name --
   ------------------

   procedure Write_C_Name (S : String; Need_Suffix : Boolean := False) is
      Append_Suffix : Boolean := Need_Suffix;
   begin
      --  First check for C predefined types and keywords. Note that we do not
      --  need to check C keywords which are also Ada reserved words since
      --  (if present) they were rejected by the Ada front end.
      --  Those keywords are: case do else for goto if return while.
      --  In this case, append an _ at the end of the name.

      if S in "auto" | "bool" | "break" | "char" | "const" | "continue" |
              "default" | "double" | "enum" | "extern" | "float" | "int" |
              "long" | "register" | "short" | "signed" | "sizeof" | "static" |
              "struct" | "switch" | "typedef" | "union" | "unsigned" | "void" |
              "volatile"
      then
         Write_Str (S);
         Append_Suffix := True;
      else
         --  We assume here that the only characters we have to be concerned
         --  about are "." and "-", which we remap to "_" and "__",
         --  respectively. GNAT LLVM itself mostly generates '.' but the LLVM
         --  optimizer may generate e.g. .pre-phixxx variables, see
         --  lib/Transforms/Scalar/GVN.cpp or xxxthread-pre-split, see
         --  lib/Transforms/Scalar/JumpThreading.cpp. However, GNAT LLVM can
         --  generate "-" in cases where the filename has a "-" and we're
         --  making file-level globals (e.g., FNAME). Because we could have
         --  two files, one with "-" and one with "_", we can't convert "-"
         --  into "_", but must double it (we could have a file named that
         --  way too, but it would give a compilation warning because the
         --  package can't be named that way, so this can't conflict).

         for C of S loop
            if C = '.' then
               Append_Suffix := True;
               Write_Char ('_');
            elsif C = '-' then
               Append_Suffix := True;
               Write_Str ("__");
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
      E : constant Entity_Id := Get_Entity (V);

   begin
      --  If it has a name, write that name and we're done. But if it's a
      --  variable not associated with a source entity, append "_" to the
      --  name since an internal name could conflict with a user name
      --  (variable vs. function). But don't do this if we aren't getting
      --  data from the front end since that would cause us to append the "_"
      --  unconditionally, which can interfere with External_Names and is
      --  more likely to cause an issue than a conflict.

      if Has_Name (V) then
         Write_C_Name
           (Get_Value_Name (V),
            not Is_A_Function (V) and then (No (E) or else Is_Type (E))
            and then Use_FE_Data);

      --  Otherwise print (and make if necessary) an internal name for this
      --  value.

      else
         --  If this is a value that's used in an inline function, we can't
         --  make it static, which means that the name must not conflict,
         --  so qualify it with our filename.

         Write_Internal_Name ("ccg_v", Get_Must_Globalize (V));
         Write_Int (Get_Output_Idx (V));
      end if;
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
            | X86_FP80_Type_Kind | FP128_Type_Kind | PPC_FP128_Type_Kind =>
            Write_Str ("0.0");

         when Integer_Type_Kind =>
            Write_Str ("0");

         when Pointer_Type_Kind =>
            Write_Str (NULL_String);

         when Struct_Type_Kind =>

            declare
               Fields_Written : Nat := 0;

            begin
               Write_Str ("{");
               for J in Nat (0) .. Count_Struct_Element_Types (T) - 1 loop
                  if not Is_Zero_Length_Array (Struct_Get_Type_At_Index (T, J))
                    and then not (Struct_Out_Style (T) = Normal
                                  and then Is_Field_Padding (T, J))
                  then
                     Maybe_Write_Comma (First);
                     Write_Undef (Struct_Get_Type_At_Index (T, J));
                     Fields_Written := Fields_Written + 1;
                  end if;
               end loop;

               --  For an empty struct, write out a value for the dummy field

               if Fields_Written = 0 then
                  Write_Str ("0");
               end if;

               Write_Str ("}");
            end;

         when Array_Type_Kind =>
            Write_Str ("{");
            for J in 0 .. Effective_Array_Length (T) - 1 loop
               Maybe_Write_Comma (First);
               Write_Undef (Get_Element_Type (T));
            end loop;

            Write_Str ("}");

         when others =>
            raise Program_Error;
      end case;
   end Write_Undef;

   --------------------------
   -- Write_Constant_Value --
   --------------------------

   procedure Write_Constant_Value
     (V             : Value_T;
      Flags         : Value_Flags := Default_Value_Flags;
      Need_Unsigned : Boolean     := False)
   is
      procedure Write_Int_Qualifier (Width : Int);
      --  Write the relevant L/LL signed int qualifier

      -------------------------
      -- Write_Int_Qualifier --
      -------------------------

      procedure Write_Int_Qualifier (Width : Int) is
      begin
         if Width = Long_Long_Size then
            Write_Str ("LL");
         elsif Width > Int_Size then
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

      --  For a struct, write the values individually

      elsif Is_A_Constant_Struct (V) then
         declare
            T              : constant Type_T             := Type_Of (V);
            Types          : constant Nat                :=
              Count_Struct_Element_Types (T);
            SOS            : constant Struct_Out_Style_T :=
              Struct_Out_Style (T);
            Fields_Written : Nat                         := 0;

         begin
            Write_Str ("{");
            pragma Assert (Types = Get_Num_Operands (V));
            for J in 0 .. Types - 1 loop

               --  If this is a zero-length array or a padding field, omit
               --  the initializer, since we don't have that field.

               if not Is_Zero_Length_Array (Struct_Get_Type_At_Index
                                              (Type_Of (V), J))
                 and then not (SOS = Normal and then Is_Field_Padding (T, J))
               then
                  Maybe_Write_Comma (First);
                  Maybe_Decl (Get_Operand (V, J), For_Initializer => True);
                  Write_Value (Get_Operand (V, J), Flags => Flags);
                  Fields_Written := Fields_Written + 1;
               end if;
            end loop;

            --  If this is a zero-length struct, add an extra item

            if Fields_Written = 0 then
               Write_Undef (Get_Element_Type (V));
            end if;

            Write_Str ("}");
         end;

      --  For an array, write the values individually

      elsif Is_A_Constant_Array (V) then
         Write_Str ("{");
         for J in 0 .. Nat'(Get_Num_Operands (V)) - 1 loop
            Maybe_Write_Comma (First);
            Maybe_Decl (Get_Operand (V, J), For_Initializer => True);
            Write_Value (Get_Operand (V, J), Flags => Flags);
         end loop;

         --  If this is a zero-length array, add an extra item

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
         Write_Str (NULL_String);

      elsif Is_Undef (V) or else Is_A_Constant_Aggregate_Zero (V) then
         Write_Undef (Type_Of (V));

      else
         raise Program_Error;
      end if;
   end Write_Constant_Value;

   -----------------
   -- Write_Value --
   -----------------

   procedure Write_Value
     (V              : Value_T;
      Flags          : Value_Flags := Default_Value_Flags;
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
         (Is_Integral_Type (V)
            and then Get_Scalar_Bit_Size (Type_Of (V)) < Get_Int_Size
            and then Is_A_Instruction (V)
            and then Get_Opcode (V) in Op_Add   | Op_Sub   | Op_Mul   |
                                       Op_U_Div | Op_S_Div | Op_U_Rem |
                                       Op_S_Rem | Op_L_Shr | Op_A_Shr |
                                       Op_Shl   | Op_And   | Op_Or    |
                                       Op_Xor);
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
      --  If we're to write the type of V instead of the value of V, do so.
      --  If this is for a decl and its an LHS, use the element type instead.

      if Flags.Write_Type then
         Write_Type ((if   Flags.LHS and then Get_Is_LHS (V)
                      then Get_Element_Type (Type_Of (V)) else Type_Of (V)),
                     Flags => (if    Flags.Need_Unsigned then +Need_Unsigned
                               elsif Flags.Need_Signed   then +Need_Signed
                               else  Default_Type_Flags),
                     V => V);
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

         --  If it's not unsigned or we need to be concerned about integer
         --  promotion, write a cast and then the value. Note that we can't
         --  use #T or Write_Type here because we often want to write a
         --  different signedness than V. Likewise below.

         elsif Must_Write_Cast or else not Is_Unsigned (V) then
            Maybe_Write_Parens;
            Write_Str ("(" & (Type_Of (V) + Need_Unsigned) & ") ");
         end if;

         --  Otherwise, if this is an object that must be interpreted as
         --  signed but might be unsigned or we need to be concerned about
         --  integer promotion, write a cast to the signed type.

      elsif Flags.Need_Signed
        and then (Is_Unsigned (V) or else Must_Write_Cast)
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

         if Get_Is_Constant (V) or else Is_Unsigned (V) then
            Maybe_Write_Parens;
            Write_Str ("(" & Type_Of (V) & ") ");
         end if;

         if not Is_Array_Type (V) then
            Maybe_Write_Parens;
            Write_Str ("&");
         end if;
      end if;

      --  If we've set an expression as the value of V, write it

      if Present (C_Value) then
         Write_Str_With_Precedence (C_Value, Inner_For_P);

      --  If this is either a simple constant or any constant except a
      --  function for an initializer, write the constant. If this is an
      --  LHS, it means that we have its address and so we want to write
      --  the name.

      elsif not Get_Is_LHS (V)
        and then (Is_Simple_Constant (V)
                    or else (Flags.Initializer and then Is_A_Constant (V)
                             and then not Is_A_Function (V)))
      then
         Write_Constant_Value (V, Flags => Flags);

      --  Otherwise, write the name

      else
         Write_Value_Name (V);
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

   -----------------
   -- Write_Type --
   -----------------

   procedure Write_Type
     (T     : Type_T;
      Flags : Type_Flags := Default_Type_Flags;
      E     : Entity_Id  := Empty;
      V     : Value_T    := No_Value_T)
   is
      procedure Write_Str_With_Signedness (S : String);
      --  Write S possibly preceeded by "unsigned".

      TE         : constant Opt_Void_Or_Type_Kind_Id :=
        (if    Present (E) then Full_Etype (E)
         elsif Present (V) then GNAT_Type (V)
         else  Empty);
      BT         : constant Opt_Void_Or_Type_Kind_Id :=
        Opt_Full_Base_Type (TE);
      RT         : Opt_Void_Or_Type_Kind_Id          := TE;
      Unsigned_P : Boolean                           := False;

      -------------------------------
      -- Write_Str_With_Signedness --
      -------------------------------

      procedure Write_Str_With_Signedness (S : String) is
      begin
         Write_Str ((if Unsigned_P then "unsigned " & S else S));
      end Write_Str_With_Signedness;

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

            --  If we force signedness, use that

            if Flags.Need_Unsigned then
               Unsigned_P := True;
            elsif Flags.Need_Signed then
               null;

            --  Next see if we have a reference that says whether this
            --  type is unsigned or not. Note that we need to check the
            --  base type to avoid unnecessary conversions.

            elsif (Present (V) and then Is_Unsigned (V))
              or else (Present (BT) and then Is_Unsigned_Type (BT))
            then
               Unsigned_P := True;
            end if;

            --  Now see if a type in Interfaces.C is in the type chain
            --  at the same size.

            while Present (RT) and then Etype (RT) /= RT
              and then RM_Size (RT) = RM_Size (Etype (RT))
            loop
               --  Now see if this is a known type in Interfaces.C. Note
               --  that we can ignore signedness here since it's been taken
               --  care of above. We really only need to worry about "long"
               --  and maybe "long long" here, since the other type sizes
               --  should be unique here, but we'll be conservative. We
               --  also have to be tricky here since "char" is an enum and
               --  hence unsigned, so we look for "signed_char" and also have
               --  to allow for derivation from both the subtype and base
               --  type of these names.
               --
               --  To save time, we check to see if this is known to be in
               --  Interfaces.C. It's tempting to do this check by calling
               --  Name_Find on "interfaces__c" and just comparing Chars,
               --  but Name_Enter is used to enter that name into the
               --  table.

               if Get_Name (Scope (Etype (RT))) = "interfaces__c" then
                  declare
                     Full_Name : constant String := Get_Name (Etype (RT));
                     Name      : constant String :=
                       Full_Name (Full_Name'First + 15 .. Full_Name'Last);

                  begin
                     if Name = "long_long" or else Name = "unsigned_long_long"
                       or else Name = "Tlong_longB"
                       or else Name = "Tunsigned_long_longB"
                     then
                        Write_Str_With_Signedness ("long long");
                        return;
                     elsif Name = "long" or else Name = "unsigned_long"
                       or else Name = "TlongB" or else Name = "Tunsigned_longB"
                     then
                        Write_Str_With_Signedness ("long");
                        return;
                     elsif Name = "int" or else Name = "unsigned_int"
                       or else Name = "TintB" or else Name = "Tunsigned_intB"
                     then
                        Write_Str_With_Signedness ("int");
                        return;
                     elsif Name = "short" or else Name = "unsigned_short"
                       or else Name = "TshortB"
                       or else Name = "Tunsigned_short"
                     then
                        Write_Str_With_Signedness ("short");
                        return;
                     elsif Name = "signed_char" or else Name = "unsigned_char"
                       or else Name = "Tsigned_charB"
                       or else Name = "Tunsigned_charB"
                     then
                        Write_Str ((if   Unsigned_P then "unsigned char"
                                    else "signed char"));
                        return;
                     end if;
                  end;
               end if;

               --  Go on to the next type in the type chain

               RT := Etype (RT);
            end loop;

            --  If nothing in Interfaces.C, write type name from size

            Write_Str
              (Int_Type_String (Pos (Get_Int_Type_Width (T)), Unsigned_P));

         when Pointer_Type_Kind =>

            --  There's no such thing in C as a function type, only a
            --  pointer to function type. So we special-case that.

            if Is_Function_Type (Get_Element_Type (T)) then
               Write_Internal_Name ("ccg_f");
               Write_Int (Get_Output_Idx (T));

            --  Otherwise, this is handled normally. We don't want to use a
            --  concatenation operator because that might cause us to try
            --  to write out the typedef for the pointed-to type, which
            --  will then be at the wrong place.

            else
               Write_Type (Get_Element_Type (T),
                           Flags => Flags,
                           E     =>
                             (if   Present (BT) and then Is_Access_Type (BT)
                              then Full_Designated_Type (BT) else Empty));
               Write_Str (" *");
            end if;

         when Struct_Type_Kind =>
            if Struct_Has_Name (T) then
               Write_C_Name (Get_Struct_Name (T));
            else
               Write_Internal_Name ("ccg_s");
               Write_Int (Get_Output_Idx (T));
            end if;

         when Array_Type_Kind =>
            Write_Internal_Name ("ccg_a");
            Write_Int (Get_Output_Idx (T));

         when others =>
            raise Program_Error;
      end case;
   end Write_Type;

   --------------------
   -- Write_BB_Value --
   --------------------

   procedure Write_BB_Value (BB : Basic_Block_T) is
   begin
      --  If it has a name, write that name. Otherwise, print (and possibly
      --  make) an internal name.

      if Has_Name (BB) then
         Write_C_Name (Get_Value_Name (BB));
      else
         Write_Str ("ccg_l");
         Write_Int (Get_Output_Idx (BB));
      end if;
   end Write_BB_Value;

   ------------------------
   -- Initialize_Writing --
   ------------------------

   procedure Initialize_Writing is
      Wrote_Include : Boolean := False;

   begin
      --  If we're not writing to standard output, open the .c or .h file

      if not Debug_Flag_Dot_YY then
         if Emit_Header then
            Create_H_File;
         else
            Create_C_File;
         end if;

         Set_Output (Output_FD);
      end if;

      --  If we're writing a header file, add test for file-specific symbol
      --  and #define for it.

      if Emit_Header then
         declare
            File   : constant String :=
              Get_Name_String (File_Name (Our_Source_File));
            Result : String (File'Range);

         begin
            for J in File'Range loop
               case File (J) is
                  when 'A' .. 'Z' | '0' .. '9' | '_' =>
                     Result (J) := File (J);
                  when 'a' .. 'z' =>
                     Result (J) := Fold_Upper (File (J));
                  when others =>
                     Result (J) := '_';
               end case;
            end loop;

            Defined_Name := +(Result & "_H");
         end;

         Write_Str ("#ifndef " & Defined_Name, Eol => True);
         Write_Str ("#define " & Defined_Name, Eol => True);
         Write_Eol;
      end if;

      --  Write the initial header info as requested

      if (not Emit_Header or else Inlines_In_Header)
        and then Have_Includes
      then
         Write_Line ("#include <string.h>");
         Write_Line ("#include <stdlib.h>");

         if Needs_Malloc_H then
            Write_Line ("#include <malloc.h>");
         end if;

         Wrote_Include := True;
      end if;

      if Use_Stdint then
         Write_Line ("#include <stdint.h>");
         Wrote_Include := True;
      end if;

      if Wrote_Include then
         Write_Eol;
      end if;

      --  If we're writing lines from the Ada source, set up our main
      --  file name and write the initial lines.

      if not Emit_Header and then Dump_Source_Text then
         Main_Source_Name :=
           +Get_Name_String (Full_Debug_Name (Our_Source_File));
         Src              := Source_Text (Our_Source_File);
         for J in Physical_Line_Number'First .. Lowest_Line_Number - 1 loop
            Write_Source_Line (J);
         end loop;

         Next_Line_To_Dump := Lowest_Line_Number;
      end if;

   end Initialize_Writing;

   ----------------------
   -- Finalize_Writing --
   ----------------------

   procedure Finalize_Writing is
   begin
      --  If we're dumping source lines, dump any that remain

      if not Emit_Header and then Dump_Source_Text then
         for J in Next_Line_To_Dump .. Last_Source_Line (Our_Source_File) loop
            Write_Source_Line (J);
         end loop;
      end if;

      --  If this is a header file, write the #endif for the protection

      if Emit_Header then
         Write_Eol;
         Write_Str ("#endif /* " & Defined_Name & "*/", Eol => True);
      end if;

      --  If we opened a file to write to, close it

      if not Debug_Flag_Dot_YY then
         if Emit_Header then
            Close_H_File;
         else
            Close_C_File;
         end if;

         Set_Standard_Output;
      end if;

      pragma Assert (Indent = 0);
   end Finalize_Writing;

   ---------------------
   -- Is_Comment_Line --
   ---------------------

   function Is_Comment_Line (L : Physical_Line_Number) return Boolean is
      Scn : Source_Ptr;

   begin
      Scn := Line_Start (L, Our_Source_File);
      while Src (Scn) = ' ' or else Src (Scn) = ASCII.HT loop
         Scn := Scn + 1;
      end loop;

      return Src (Scn) in Line_Terminator | EOF
        or else Src (Scn .. Scn + 1) = "--";
   end Is_Comment_Line;

   -----------------------
   -- Write_Source_Line --
   -----------------------

   procedure Write_Source_Line (L : Physical_Line_Number) is
      Scn       : Source_Ptr;
      Last_Char : Character := ' ';

   begin
      if Is_Comment_Line (L) then
         return;
      end if;

      Write_Str ("/* ");
      Write_Int (Nat (L));
      Write_Str (": ");

      Scn := Line_Start (L, Our_Source_File);
      while Scn <= Src'Last and then Src (Scn) not in Line_Terminator loop

         --  We have the pathological case of a Ada source line containing
         --  a "*/" string, which will cause the C comment line we're writing
         --  to be ended prematurely. So write "*_/" instead in that case.

         if Last_Char = '*' and then Src (Scn) = '/' then
            Write_Char ('_');
         end if;

         Write_Char (Src (Scn));
         Last_Char := Src (Scn);
         Scn := Scn + 1;
      end loop;

      Write_Line (" */");
   end Write_Source_Line;

   ------------------
   -- Write_C_Line --
   ------------------

   procedure Write_C_Line
     (Idx : Stmt_Idx; Start_Block, End_Block : Block_Style := None)
   is
      OL         : Out_Line    := Get_Stmt_Line (Idx);
      Second_End : Block_Style := None;

   begin
      --  The only time we override the block style at the start of a block
      --  is the first statement of a basic block and it can't be a
      --  start block, but verify that.

      if Present (Start_Block) then
         pragma Assert (No (OL.Start_Block));
         OL.Start_Block := Start_Block;
      end if;

      --  If we're overriding the end block, we may already be ending a block
      --  that was opened earlier. In that case, we need to end both blocks.

      if Present (End_Block) then
         if Present (OL.End_Block) then
            Second_End := End_Block;
         else
            OL.End_Block := End_Block;
         end if;
      end if;

      Write_C_Line (OL);
      Write_End_Block (Second_End, False);
   end Write_C_Line;

   ------------------
   -- Write_C_Line --
   ------------------

   procedure Write_C_Line (Idx : Typedef_Idx) is
   begin
      Write_C_Line (Get_Typedef_Line (Idx));
   end Write_C_Line;

   ------------------
   -- Write_C_Line --
   ------------------

   procedure Write_C_Line (Idx : Global_Decl_Idx) is
   begin
      Write_C_Line (Get_Global_Decl_Line (Idx));
   end Write_C_Line;

   ------------------
   -- Write_C_Line --
   ------------------

   procedure Write_C_Line (Idx : Local_Decl_Idx) is
   begin
      Write_C_Line (Get_Local_Decl_Line (Idx));
   end Write_C_Line;

   ------------------
   -- Write_C_Line --
   ------------------

   procedure Write_C_Line
     (S             : Str;
      Indent_Type   : Indent_Style  := Normal;
      End_Block     : Block_Style   := None;
      V             : Value_T       := No_Value_T;
      No_Debug_Info : Boolean       := False)
   is
   begin
      Write_C_Line (Out_Line'(Line_Text     => S,
                              Start_Block   => None,
                              End_Block     => End_Block,
                              Indent_Type   => Indent_Type,
                              V             => V,
                              No_Debug_Info => No_Debug_Info));
   end Write_C_Line;

   ------------------
   -- Write_C_Line --
   ------------------

   procedure Write_C_Line
     (S             : String;
      Indent_Type   : Indent_Style  := Normal;
      End_Block     : Block_Style   := None;
      V             : Value_T       := No_Value_T;
      No_Debug_Info : Boolean       := False)
   is
   begin
      Write_C_Line (Out_Line'(Line_Text     => +S,
                              Start_Block   => None,
                              End_Block     => End_Block,
                              Indent_Type   => Indent_Type,
                              V             => V,
                              No_Debug_Info => No_Debug_Info));
   end Write_C_Line;

   ------------------
   -- Write_C_Line --
   ------------------

   procedure Write_C_Line (OL : Out_Line) is
      Our_V         : constant Value_T              :=
        (if   No (OL.V) or else OL.No_Debug_Info then No_Value_T
           elsif Is_A_Instruction (OL.V) or else Is_A_Function (OL.V)
                 or else Is_A_Global_Variable (OL.V) then OL.V
         else No_Value_T);
      Our_File      : constant Str                  :=
        (if   Present (Our_V) and then Emit_Debug_Info
         then +Get_Debug_Loc_Filename (Our_V) else No_Str);
      Our_Dir       : constant Str                  :=
        (if   Present (Our_V) and then Dump_Source_Text
         then +Get_Debug_Loc_Directory (Our_V) else No_Str);
      In_Main       : constant Boolean              :=
        Present (Our_Dir) and then Our_Dir & Our_File = Main_Source_Name;
      Have_File     : constant Boolean              :=
        Present (Our_File) and then not Is_Null_String (Our_File);
      Our_Line      : constant Physical_Line_Number :=
        (if Have_File then +Get_Debug_Loc_Line (Our_V) else 1);
      End_Block     : Block_Style                   := OL.End_Block;
      S             : Str                           := OL.Line_Text;
      Our_Indent    : Integer;

   begin
      --  If we have debug info and it differs from the last we have, and
      --  we are to write a #line, write it and update our last line
      --  written.

      if Have_File and then Emit_C_Line
        and then (Our_File /= Previous_Debug_File
                    or else Our_Line /= Previous_Debug_Line)
      then
         Write_Str ("#line " & Nat (Our_Line) & " """ & Our_File & """",
                    Eol => True);
         Previous_Debug_File := Our_File;
         Previous_Debug_Line := Our_Line;
      end if;

      --  If we're to write source lines, we're in the main file and
      --  this position is higher than all the source lines we've written
      --  so far, write out all lines up to ours.

      if Dump_Source_Text and then In_Main
        and then Our_Line >= Next_Line_To_Dump
      then
         for J in Next_Line_To_Dump .. Our_Line loop
            Write_Source_Line (J);
         end loop;

         Next_Line_To_Dump := Our_Line + 1;
      end if;

      --  If our last line ended a block and this one neither ends a block,
      --  starts with "else", or is itself a blank line, write a blank
      --  line.

      if Previous_Was_End_Block and then Present (S) and then No (End_Block)
        and then not Is_First_Char (S, '}')
        and then not Starts_With (S, "else") and then not Is_Null_String (S)
      then
         Write_Eol;
      end if;

      --  Now handle possibly starting a block, write our line, then
      --  possibly ending a block. Handle any special indentation
      --  requirements. We special-case having start line starting with
      --  "{". In that case, we use that as the line to write.

      if Present (OL.Start_Block) and then Is_First_Char (S, '{') then
         Write_Start_Block (OL.Start_Block, False, S);
         S := No_Str;
      else
         Write_Start_Block (OL.Start_Block, Present (End_Block));
      end if;

      --  Process special indentation for this line

      Our_Indent := Indent;

      if OL.Indent_Type = Left then
         Our_Indent := 0;
      elsif OL.Indent_Type = Under_Brace then
         Our_Indent := Indent - C_Indent;
      end if;

      --  We special-case having an end line starting with "}". In that
      --  case, we use that as the line to write.

      if Present (OL.End_Block) and then Present (S)
        and then Is_First_Char (S, '}')
      then
         Write_End_Block (End_Block, False, S);
         End_Block := None;
      elsif Present (S) then
         Write_Str ((Our_Indent * " ") & S, Eol => True);
      end if;

      Previous_Was_End_Block :=
        Present (End_Block)
        or else (Present (S) and then Is_First_Char (S, '}'));
      Write_End_Block (End_Block, Present (OL.Start_Block));
   end Write_C_Line;

end CCG.Write;
