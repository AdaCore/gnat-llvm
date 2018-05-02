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

with Errout;   use Errout;
with Output;   use Output;
with Sem_Mech; use Sem_Mech;
with Sem_Aux;  use Sem_Aux;
with Sprint;   use Sprint;
with Stringt;  use Stringt;

with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.Types;       use GNATLLVM.Types;
with GNATLLVM.Wrapper;     use GNATLLVM.Wrapper;

package body GNATLLVM.Utils is

   procedure Add_Type_Data_To_Instruction (Inst : Value_T; TE : Entity_Id);
   --  Helper to add type data (e.g., volatility and TBAA info) to
   --  an Instruction.

   ------------------------
   -- Is_Constant_Folded --
   ------------------------
   function Is_Constant_Folded (E : Entity_Id) return Boolean is
     (Ekind (E) = E_Constant
        and then Is_Scalar_Type (Get_Full_View (Full_Etype (E))));

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
               Low := Get_Uint_Value (N);
               High := Low;
            end if;

         when N_Subtype_Indication =>
            Decode_Range (Range_Expression (Constraint (N)), Low, High);

         when N_Range | N_Signed_Integer_Type_Definition =>
            Low := Get_Uint_Value (Low_Bound (N));
            High := Get_Uint_Value (High_Bound (N));

         when N_Character_Literal | N_Integer_Literal =>
            Low := Get_Uint_Value (N);
            High := Low;

         when others =>
            Error_Msg_N ("unknown range operand", N);
            Low := No_Uint;
            High := No_Uint;
      end case;
   end Decode_Range;

   ---------
   -- Img --
   ---------

   function Img (I : Nat) return String is
      Str : constant String := I'Image;
   begin
      return Str (Str'First + 1 .. Str'Last);
   end Img;

   -----------------
   -- First_Field --
   -----------------

   function First_Field (E : Entity_Id) return Entity_Id is
      Field : Entity_Id := First_Entity (E);
   begin
      while Present (Field)
        and then not Ekind_In (Field, E_Discriminant, E_Component)
      loop
         Next_Entity (Field);
      end loop;

      return Field;
   end First_Field;

   ----------------
   -- Next_Field --
   ----------------

   function Next_Field (E : Entity_Id) return Entity_Id is
      Field : Entity_Id := Next_Entity (E);
   begin
      while Present (Field)
        and then not Ekind_In (Field, E_Discriminant, E_Component)
      loop
         Next_Entity (Field);
      end loop;

      return Field;
   end Next_Field;

   ----------------
   -- Next_Field --
   ----------------
   procedure Next_Field (E : in out Entity_Id) is
   begin
      E := Next_Field (E);
   end Next_Field;

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

   function Get_Uint_Value (N : Node_Id) return Uint is
      E : Entity_Id;

   begin
      case Nkind (N) is
         when N_Character_Literal =>
            --  If a Entity is present, it means that this was one of the
            --  literals in a user-defined character type.

            return
              (if Present (Entity (N)) then Enumeration_Rep (Entity (N))
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

   ---------------------
   -- Param_Needs_Ptr --
   ---------------------

   function Param_Needs_Ptr (Param : Entity_Id) return Boolean is
      TE : constant Entity_Id := Full_Etype (Param);

   begin
      --  ??? Return True for all array types for now

      if Is_Array_Type (TE) then
         return True;

      --  Pass records by reference when using the default mechanism, otherwise
      --  this will cause an inefficient pass C struct by copy which is not
      --  what users expect by default.

      elsif Is_Record_Type (TE)
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

   function Access_Depth (TE : Entity_Id) return Natural
     with Pre => Present (TE);
   --  If TE is not an access type, return zero.  Otherwise, return how deep
   --  we have to go down Full_Designated_Type to find something that's
   --  not an access type.

   function Access_Depth (V : GL_Value) return Natural
     with Pre => Present (V);
   --  Similarly, but for a GL_Value, which might be a reference

   function Is_LValue_Of (V : GL_Value; TE : Entity_Id) return Boolean
     with Pre => Present (V) and then Is_Type_Or_Void (TE);
   --  Return True if G is the LValue of an object of type TE

   ------------------
   -- Access_Depth --
   ------------------

   function Access_Depth (TE : Entity_Id) return Natural is
   begin
      if not Is_Access_Type (TE) then
         return 0;
      else
         return Access_Depth (Full_Designated_Type (TE)) + 1;
      end if;
   end Access_Depth;

   ------------------
   -- Access_Depth --
   ------------------

   function Access_Depth (V : GL_Value) return Natural is
   begin
      if not Is_Access_Type (V) then
         return 0;
      else
         return Access_Depth (Full_Designated_Type (V)) + 1;
      end if;
   end Access_Depth;

   ------------------
   -- Is_LValue_Of --
   ------------------

   function Is_LValue_Of (V : GL_Value; TE : Entity_Id) return Boolean is
   begin
      --  If Value is a reference and its designated type is that of our
      --  type, we know we're OK.  If not, we may still if OK if access
      --  types are involved, so check that the "access type" depth of
      --  Value is one greater than that of our type.  That's also OK.
      --  And if what we have is a subprogram, we're also OK.

      return Is_Subprogram_Type (V)
        or else (Is_Reference (V)
                   and then (Full_Designated_Type (V) = TE
                               or else (Ekind (Full_Designated_Type (V))
                                          = E_Subprogram_Type)))
        or else Access_Depth (V) = Access_Depth (TE) + 1;
   end Is_LValue_Of;

   ----------------
   -- Need_Value --
   ----------------

   function Need_Value (V : GL_Value; TE : Entity_Id) return GL_Value is
   begin
      --  If V is of dynamic size, the "value" we use is the
      --  reference, so return it.  Similarly for subprograms.
      --  Likewise if it's not a reference.  Otherwise, load the
      --  value.

      if Ekind (TE) = E_Subprogram_Type or else not Is_LValue_Of (V, TE)
        or else (Is_Reference (V)
                   and then Is_Dynamic_Size (Full_Designated_Type (V)))
      then
         return V;
      else
         return Load (V);
      end if;
   end Need_Value;

   -----------------
   -- Need_LValue --
   -----------------

   function Need_LValue
     (V : GL_Value; TE : Entity_Id; Name : String := "lvalue")
     return GL_Value
   is
      Alloc_Type : Entity_Id;
   begin

      --  Compute the type to use for the allocation.  If V isn't
      --  present, we have no options.  If it is, figure out what its
      --  type is.

      if No (V) then
         Alloc_Type := TE;
      elsif Has_Known_Etype (V) then
         Alloc_Type := Full_Etype (V);
      elsif Is_Reference (V) and not Is_Raw_Array (V) then
         Alloc_Type := Full_Designated_Type (V);
      else
         Alloc_Type := TE;
      end if;

      --  If at top level or we already have an LValue, return it.
      --  Otherwise, allocate memory for the value (which we know to be of
      --  fixed size or else we'd have had a reference).

      if Library_Level or else Is_LValue_Of (V, TE) then
         return V;
      else
         pragma Assert (not Library_Level);

         return Allocate_For_Type (TE, Alloc_Type, V => V, Name => Name);
      end if;
   end Need_LValue;

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
      if (Is_Imported (E) or else Is_Exported (E))
        and then Present (Interface_Name (E))
        and then No (Address_Clause (E))
      then
         Append (Buf, Strval (Interface_Name (E)));
         return +Buf;
      else
         return Get_Name_String (Chars (E));
      end if;
   end Get_Ext_Name;

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
      Dump_LLVM_Value (V.Value);
      Dump_LLVM_Type (Type_Of (V.Value));
      if Is_Reference (V) then
         Write_Str ("Is_Reference ");
         if Is_Raw_Array (V) then
            Write_Str ("Is_Raw_Array");
         end if;

         Write_Eol;
      end if;

      pg (Union_Id (V.Typ));
   end Dump_GL_Value;

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

      procedure Dump_LLVM_Type_C (T : Type_T);
      pragma Import (C, Dump_LLVM_Type_C, "Dump_LLVM_Type_C");

   begin
      Dump_LLVM_Type_C (T);
   end Dump_LLVM_Type;

   pragma Annotate (Xcov, Exempt_Off, "Debug helpers");

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

   ----------------------------------
   -- Add_Type_Data_To_Instruction --
   ----------------------------------

   procedure Add_Type_Data_To_Instruction (Inst : Value_T; TE : Entity_Id)
   is
      TBAA : constant Metadata_T := Get_TBAA (Implementation_Base_Type (TE));
   begin
      if Is_Volatile (TE) then
         Set_Volatile (Inst);
      end if;

      if Present (TBAA) then
         Add_TBAA_Access
           (Inst, Create_TBAA_Access_Tag (MD_Builder, TBAA, TBAA, 0));
      end if;
   end Add_Type_Data_To_Instruction;

   --------------------
   -- Load_With_Type --
   --------------------

   function Load_With_Type (TE : Entity_Id; Ptr : Value_T; Name : String := "")
     return Value_T
   is
      Load_Inst : constant Value_T := Load (IR_Builder, Ptr, Name);

   begin
      Add_Type_Data_To_Instruction (Load_Inst, TE);
      return Load_Inst;
   end Load_With_Type;

   -----------
   -- Store --
   -----------

   procedure Store (Bld : Builder_T; Expr : Value_T; Ptr : Value_T) is
      Dummy : Value_T;

   begin
      Dummy := Build_Store (Bld, Expr, Ptr);
   end Store;

   ---------------------
   -- Store_With_Type --
   ---------------------
   procedure Store_With_Type (TE : Entity_Id; Expr : Value_T; Ptr : Value_T)
   is
      Store_Inst : constant Value_T := Build_Store (IR_Builder, Expr, Ptr);

   begin
      Add_Type_Data_To_Instruction (Store_Inst, TE);
   end Store_With_Type;

end GNATLLVM.Utils;
