------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
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

with Interfaces.C; use Interfaces.C;

with Sinfo;      use Sinfo;

with LLVM.Core;  use LLVM.Core;

with GNATLLVM.GLValue;     use GNATLLVM.GLValue;
with GNATLLVM.Types;       use GNATLLVM.Types;
with GNATLLVM.Utils;       use GNATLLVM.Utils;

package body GNATLLVM.Records is

   function Rec_Comp_Filter (E : Entity_Id) return Boolean is
     (Ekind (E) in E_Component | E_Discriminant);

   function Iterate_Components is new Iterate_Entities
     (Get_First => First_Entity,
      Get_Next  => Next_Entity,
      Filter    => Rec_Comp_Filter);
   --  Iterate over all components of a given record type

   function Create_Record_Type (Def_Ident : Entity_Id) return Type_T is
      Struct_Type   : Type_T;
      Comps         : constant Entity_Iterator :=
        Iterate_Components (Def_Ident);
      LLVM_Comps    : array (1 .. Comps'Length) of Type_T;
      I             : Natural := 1;
      Struct_Num    : Nat := 1;
      Num_Fields    : Natural := 0;
      Info          : Record_Info;
      Fields        : Field_Info_Vectors.Vector;
      Current_Field : Field_Info;

      function New_Struct_Info return Struct_Info is
         ((LLVM_Type => Struct_Type, Preceding_Fields => Fields));

   begin
      Struct_Type := Struct_Create_Named (Env.Ctx, Get_Name (Def_Ident));
      Info.Structs.Append (New_Struct_Info);

      --  Records enable some "type recursivity", so store this one in the
      --  environment so that there is no infinite recursion when nested
      --  components reference it.

      Set_Type (Def_Ident, Struct_Type);

      for Comp of Comps loop
         LLVM_Comps (I) := Create_Type (Full_Etype (Comp));
         Current_Field := (Struct_Num, Nat (I - 1), Comp, LLVM_Comps (I));
         Fields.Append (Current_Field);
         Info.Fields.Include (Comp, Current_Field);
         I := I + 1;
         Num_Fields := Num_Fields + 1;

         --  If we are on a component with a dynamic size,
         --  we create a new struct type for the following components.

         if Is_Dynamic_Size (Full_Etype (Comp)) then
            Info.Dynamic_Size := True;
            Struct_Set_Body (Struct_Type, LLVM_Comps'Address,
                             unsigned (I - 1), False);
            I := 1;
            Struct_Num := Struct_Num + 1;

            Struct_Type := Struct_Create_Named
              (Env.Ctx, Get_Name (Def_Ident) & Img (Struct_Num));
            Info.Structs.Append (New_Struct_Info);
         end if;
      end loop;

      Struct_Set_Body
        (Struct_Type, LLVM_Comps'Address, unsigned (I - 1), False);
      Set_Record_Info (Def_Ident, Info);
      Set_Dynamic_Size (Def_Ident, Info.Dynamic_Size);
      return Get_Type (Def_Ident);

   end Create_Record_Type;

   -------------------------
   -- Record_Field_Offset --
   -------------------------

   function Record_Field_Offset
     (Record_Ptr   : Value_T;
      Record_Field : Node_Id) return Value_T
   is
      Type_Id    : constant Entity_Id :=
        Get_Fullest_View (Scope (Record_Field));
      R_Info     : constant Record_Info := Get_Record_Info (Type_Id);
      F_Info     : constant Field_Info := R_Info.Fields.Element (Record_Field);
      Struct_Ptr : Value_T := Record_Ptr;

   begin
      if F_Info.Containing_Struct_Index > 1 then
         declare
            Int_Struct_Address : Value_T := Ptr_To_Int
              (Env.Bld,
               Record_Ptr, Int_Ptr_Type, "offset-calc");
            S_Info : constant Struct_Info :=
              R_Info.Structs (F_Info.Containing_Struct_Index);

         begin
            --  Accumulate the size of every field

            for Preceding_Field of S_Info.Preceding_Fields loop
               Int_Struct_Address := NSW_Add
                 (Env.Bld,
                  Int_Struct_Address,
                  Get_Type_Size
                    (Full_Etype (Preceding_Field.Entity), No_GL_Value).Value,
                  "offset-calc");
            end loop;

            Struct_Ptr := Int_To_Ptr
              (Env.Bld,
               Int_Struct_Address, Pointer_Type (S_Info.LLVM_Type, 0), "back");
         end;
      end if;

      return Struct_GEP
        (Env.Bld,
         Struct_Ptr, unsigned (F_Info.Index_In_Struct), "field_access");
   end Record_Field_Offset;

   ------------------------------
   -- Record_With_Dynamic_Size --
   ------------------------------

   function Record_With_Dynamic_Size (T : Entity_Id) return Boolean
   is
      Full_View : constant Entity_Id := Get_Fullest_View (T);
      Unused    : Type_T;

   begin
      if Is_Record_Type (Full_View) then
         --  First ensure the type is created
         Unused := Create_Type (Full_View);
         return Get_Record_Info (Full_View).Dynamic_Size;
      else
         return False;
      end if;
   end Record_With_Dynamic_Size;

   --------------------------
   -- Get_Record_Type_Size --
   --------------------------

   function Get_Record_Type_Size
     (TE       : Entity_Id;
      V        : GL_Value;
      For_Type : Boolean := False) return GL_Value
   is
      Dynamic_Fields : Boolean := False;
      T              : constant Type_T := Create_Type (TE);
      Size           : GL_Value := Get_LLVM_Type_Size (T);
      pragma Unreferenced (V);

   begin
      if Record_With_Dynamic_Size (TE) then
         for Comp of Iterate_Components (TE) loop
            if Is_Dynamic_Size (Full_Etype (Comp)) then
               Dynamic_Fields := True;
            end if;

            --  Compute size of all fields once we've found a dynamic
            --  component.

            if Dynamic_Fields then
               Size := NSW_Add
                 (Size,
                  Get_Type_Size (Full_Etype (Comp), No_GL_Value, For_Type),
                  "record-size");
            end if;
         end loop;
      end if;

      return Size;
   end Get_Record_Type_Size;

end GNATLLVM.Records;
