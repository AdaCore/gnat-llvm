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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Einfo;    use Einfo;
with Sem_Mech; use Sem_Mech;
with Stringt;  use Stringt;

with GNATLLVM.Wrapper;    use GNATLLVM.Wrapper;

package body GNATLLVM.Utils is

   procedure Add_Type_Data_To_Instruction
     (Env : Environ; Inst : Value_T; TE : Entity_Id);
   --  Helper to add type data (e.g., volatility and TBAA info) to
   --  an Instruction.

   ---------
   -- Img --
   ---------

   function Img (I : Nat) return String is
      Str : constant String := I'Image;
   begin
      return Str (Str'First + 1 .. Str'Last);
   end Img;

   ---------------------
   -- Get_Param_Types --
   ---------------------

   function Get_Param_Types (Fn_Ty : Type_T) return Type_Array is
      Fn_Ty_Real : constant Type_T :=
        (if Get_Type_Kind (Fn_Ty) = Pointer_Type_Kind
         then Get_Element_Type (Fn_Ty)
         else Fn_Ty);
      Params_Types : Type_Array (1 .. Nat (Count_Param_Types (Fn_Ty_Real)));
   begin
      Get_Param_Types (Fn_Ty_Real, Params_Types'Address);
      return Params_Types;
   end Get_Param_Types;

   -----------------------------
   -- UI_To_Long_Long_Integer --
   -----------------------------

   function UI_To_Long_Long_Integer (U : Uint) return Long_Long_Integer is
   begin
      --  ??? Consider making this routine more efficient
      UI_Image (U, Decimal);
      return Long_Long_Integer'Value (UI_Image_Buffer (1 .. UI_Image_Length));
   end UI_To_Long_Long_Integer;

   --------------------
   -- Get_Uint_Value --
   --------------------

   function Get_Uint_Value (Node : Node_Id) return Uint is
      E : Entity_Id;
   begin
      case Nkind (Node) is
         when N_Character_Literal =>
            return Char_Literal_Value (Node);

         when N_Integer_Literal =>
            return Intval (Node);

         when N_Real_Literal =>

            --  We can only do something here if this is a fixed-point type.

            if Is_Fixed_Point_Type (Full_Etype (Node)) then
               return Corresponding_Integer_Value (Node);
            else
               return No_Uint;
            end if;

         when N_Identifier =>

            --  If an N_Identifier is static, its N_Defining_Identifier is
            --  either an E_Constant or an E_Enumeration_Literal.

            E := Entity (Node);
            if Ekind (E) = E_Constant then
               return Get_Uint_Value (Expression (Parent (E)));
            elsif Ekind (E) = E_Enumeration_Literal then
               return Enumeration_Rep (E);
            else
               return No_Uint;
            end if;

         when others =>
            return No_Uint;
      end case;
   end Get_Uint_Value;

   ---------------
   -- Is_LValue --
   ---------------

   function Is_LValue (Node : Node_Id) return Boolean is
      N : Node_Id := Node;
   begin
      loop
         case Nkind (N) is
            when N_Explicit_Dereference =>
               return True;
            when N_Selected_Component | N_Indexed_Component | N_Slice =>
               N := Prefix (N);
            when N_Identifier =>
               N := Entity (N);
            when N_Defining_Identifier =>
               if Present (Renamed_Object (N)) then
                  N := Renamed_Object (N);
               else
                  return True;
               end if;
            when others =>
               return False;
         end case;
      end loop;
   end Is_LValue;

   ---------------------
   -- Param_Needs_Ptr --
   ---------------------

   function Param_Needs_Ptr (Param : Entity_Id) return Boolean is
      Typ : constant Entity_Id := Full_Etype (Param);

   begin
      --  ??? Return True for all array types for now

      if Is_Array_Type (Typ) then
         return True;

      --  Pass records by reference when using the default mechanism, otherwise
      --  this will cause an inefficient pass C struct by copy which is not
      --  what users expect by default.

      elsif Is_Record_Type (Typ)
        and then Mechanism (Param) = Default_Mechanism
      then
         return True;

      elsif Ekind_In (Param, E_In_Out_Parameter, E_Out_Parameter) then
         return True;

      --  ??? Missing cases of e.g. dynamic size objects, ...

      else
         return Mechanism (Param) = By_Reference;
      end if;
   end Param_Needs_Ptr;

   ----------------------------
   -- Return_Needs_Sec_Stack --
   ----------------------------

   function Return_Needs_Sec_Stack (Arg : Node_Id) return Boolean is
      pragma Unreferenced (Arg);
   begin
      return False;
   end Return_Needs_Sec_Stack;

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

   ----------------------
   -- Iterate_Entities --
   ----------------------

   function Iterate_Entities (Root : Entity_Id) return Entity_Iterator is
      Len : Nat := 0;
      Cur : Entity_Id := Get_First (Root);
   begin
      while Cur /= Empty loop
         Cur := Get_Next (Cur);
         Len := Len + 1;
      end loop;

      declare
         A : Entity_Iterator (1 .. Len);
         I : Nat := 1;
      begin
         Cur := Get_First (Root);
         while Cur /= Empty loop
            if Filter (Cur) then
               A (I) := Cur;
               I := I + 1;
            end if;
            Cur := Get_Next (Cur);
         end loop;
         return A (1 .. I - 1);
      end;
   end Iterate_Entities;

   ----------------
   -- Get_Params --
   ----------------

   function Get_Params (Subp : Entity_Id) return Entity_Iterator is
      function Iterate is new Iterate_Entities
        (Get_First => First_Formal_With_Extras,
         Get_Next  => Next_Formal_With_Extras);

   begin
      return Iterate (Subp);
   end Get_Params;

   --------------------------
   -- Get_Subprog_Ext_Name --
   --------------------------

   function Get_Subprog_Ext_Name (E : Entity_Id) return String is
      Buf : Bounded_String;
   begin
      if (Is_Imported (E) or else Is_Exported (E))
        and then Present (Interface_Name (E))
        and then No (Address_Clause (E))
      then
         Append (Buf, Strval (Interface_Name (E)));
         return +Buf;
      else
         return Get_Name_String (Chars (E));
      end if;
   end Get_Subprog_Ext_Name;

   pragma Annotate (Xcov, Exempt_On, "Debug helpers");

   ---------------------
   -- Dump_LLVM_Value --
   ---------------------

   procedure Dump_LLVM_Value (V : Value_T) is
   begin
      Dump_Value (V);
   end Dump_LLVM_Value;

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

      --  LLVM::Type::dump is not bound in the C API and thus is not available
      --  in the Ada bindings. Hack instead.

      type Type_Class is limited null record;
      pragma Import (CPP, Type_Class);
      type Type_Class_Ptr is access Type_Class;

      procedure Dump (This : Type_Class_Ptr);
      pragma Import (CPP, Dump, "_ZNK4llvm4Type4dumpEv");

      function Unwrap is new Ada.Unchecked_Conversion
        (Type_T, Type_Class_Ptr);

   begin
      Dump (Unwrap (T));
      New_Line (Current_Error);
   end Dump_LLVM_Type;

   pragma Annotate (Xcov, Exempt_Off, "Debug helpers");

   ---------
   -- GEP --
   ---------

   function GEP
     (Bld : Builder_T; Ptr : Value_T; Indices : Value_Array; Name : String)
      return Value_T
   is
     (GEP (Bld, Ptr, Indices'Address, Indices'Length, Name));

   ----------------------------------
   -- Add_Type_Data_To_Instruction --
   ----------------------------------

   procedure Add_Type_Data_To_Instruction
     (Env : Environ; Inst : Value_T; TE : Entity_Id)
   is
      TBAA : constant Metadata_T :=
        Get_TBAA (Env, Implementation_Base_Type (TE));
   begin
      if Is_Volatile (TE) then
         Set_Volatile (Inst);
      end if;

      if TBAA /= No_Metadata_T then
         Add_TBAA_Access
           (Inst, Create_TBAA_Access_Tag (Env.MDBld, TBAA, TBAA, 0));
      end if;
   end Add_Type_Data_To_Instruction;

   --------------------
   -- Load_With_Type --
   --------------------
   function Load_With_Type
     (Env : Environ; TE : Entity_Id; Ptr : Value_T) return Value_T
   is
      Load_Inst : constant Value_T := Load (Env.Bld, Ptr, "");
   begin
      Add_Type_Data_To_Instruction (Env, Load_Inst, TE);
      return Load_Inst;
   end Load_With_Type;

   -----------
   -- Store --
   -----------

   procedure Store (Bld : Builder_T; Expr : Value_T; Ptr : Value_T)
   is
      Dummy : Value_T;
   begin
      Dummy := Build_Store (Bld, Expr, Ptr);
   end Store;

   ---------------------
   -- Store_With_Type --
   ---------------------
   procedure Store_With_Type
     (Env : Environ; TE : Entity_Id; Expr : Value_T; Ptr : Value_T)
   is
      Store_Inst : constant Value_T := Build_Store (Env.Bld, Expr, Ptr);
   begin
      Add_Type_Data_To_Instruction (Env, Store_Inst, TE);
   end Store_With_Type;

end GNATLLVM.Utils;
