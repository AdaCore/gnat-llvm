------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2013-2020, AdaCore                     --
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

with CCG.Helper; use CCG.Helper;

package body CCG.Output is

   function Is_Simple_Constant (V : Value_T) return Boolean is
     (Get_Value_Kind (V) in Constant_Int_Value_Kind | Constant_FP_Value_Kind)
     with Pre => Present (V);
   --  True if this is a simple enough constant that we output it in C
   --  source as a constant.

   procedure Write_Value_Name (V : Value_T)
     with Pre => Present (V);
   --  Write the value name of V, which is either the LLVM name or a name
   --  we generate from a serial number.

   procedure Write_Constant_Value (V : Value_T)
     with Pre => Present (Is_A_Constant (V));
   --  Write the constant value of V

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
               Write_Str (S);
               return;
            end if;

            Set_No_Name (V);
         end;
      end if;

      --  Print (and make if necessary) an internal name for this value

      Write_Str ("ccg_v");
      Write_Int (Get_Output_Idx (V));

   end Write_Value_Name;

   --------------------------
   -- Write_Constant_Value --
   --------------------------

   procedure Write_Constant_Value (V : Value_T) is
      subtype LLI is Long_Long_Integer;
   begin
      --  ??? Start with just small integer constants and FP constants

      if Present (Is_A_Constant_Int (V)) then
         declare
            Val : constant LLI := Const_Int_Get_S_Ext_Value (V);

         begin
            if Val in LLI (Int'First) .. LLI (Int'Last) then
               Write_Int (Int (Val));
            else
               Write_Str ("<overflow>");
            end if;
         end;

      elsif Present (Is_A_Constant_FP (V)) then

         declare
            Loses_Info : Boolean;

         begin
            --  ??? It's not clear that 'Image will always do the right thing
            --  in terms of writing the proper format for a C constant,
            --  but it's at least good enough to start with and there's no
            --  obvious other mechanism.

            Write_Str
              (Double'Image (Const_Real_Get_Double (V, Loses_Info)));
         end;

      else
         Write_Str ("<unknown constant>");
      end if;
   end Write_Constant_Value;

   -----------------
   -- Write_Value --
   -----------------

   procedure Write_Value (V : Value_T) is
      C_Value : constant Str := Get_C_Value (V);

   begin
      --  If we've set an expression as the value of V, write it

      if Present (C_Value) then
         Write_Str (C_Value);

      --  If this is a simple constant, write the constant

      elsif Is_Simple_Constant (V) then
         Write_Constant_Value (V);

      --  Otherwise, write the name

      else
         Write_Value_Name (V);
      end if;

   end Write_Value;

   ----------------
   -- Write_Decl --
   ----------------

   procedure Write_Decl (V : Value_T) is
   begin
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
            Write_Str ("float");

         when Integer_Type_Kind =>
            declare
               Bits : constant Pos := Pos (Get_Int_Type_Width (T));

            begin
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
                  Write_Str ("<unknown int type>");
               end if;
            end;

         when Pointer_Type_Kind =>
            Write_Str (" " & Get_Element_Type (T));

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
   end Write_Typedef;

   --------------
   -- Write_BB --
   --------------

   procedure Write_BB (B : Basic_Block_T) is
   begin
      Write_Str ("ccg_l");
      Write_Int (Get_Output_Idx (B));
   end Write_BB;

end CCG.Output;
