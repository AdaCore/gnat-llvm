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

with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

with Exp_Unst; use Exp_Unst;
with Namet;    use Namet;
with Sem_Util; use Sem_Util;
with Sinput;   use Sinput;
with Stand;    use Stand;

with LLVM.Core;  use LLVM.Core;
with LLVM.Types; use LLVM.Types;

with GNATLLVM.Compile;   use GNATLLVM.Compile;
with GNATLLVM.DebugInfo; use GNATLLVM.DebugInfo;
with GNATLLVM.GLValue;   use GNATLLVM.GLValue;
with GNATLLVM.Types;     use GNATLLVM.Types;
with GNATLLVM.Utils;     use GNATLLVM.Utils;

package body GNATLLVM.Subprograms is

   type Intrinsic is record
      Name  : access String;
      Width : Uint;
      Func  : GL_Value;
   end record;
   --  A description of an intrinsic function that we've created

   --  Since we aren't going to be creating all that many different
   --  intrinsic functions, a simple list that we search should be
   --  fast enough.

   package Intrinsic_Functions_Table is new Table.Table
     (Table_Component_Type => Intrinsic,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 5,
      Table_Name           => "Intrinsic_Function_Table");

   ---------------------
   -- Build_Intrinsic --
   ---------------------

   function Build_Intrinsic
     (Kind : Overloaded_Intrinsic_Kind;
      Name : String;
      TE   : Entity_Id) return GL_Value
   is
      Width         : constant Uint := RM_Size (TE);
      Full_Name     : constant String := Name & UI_Image (Width);
      Void_Ptr_Type : constant Type_T := Pointer_Type (Int_Ty (8), 0);
      Void_Type     : constant Type_T := Void_Type_In_Context (Env.Ctx);
      LLVM_Typ      : constant Type_T := Create_Type (TE);
      Return_TE     : Entity_Id := TE;
      Fun_Ty        : Type_T;
      Result        : GL_Value;

   begin
      for I in 1 .. Intrinsic_Functions_Table.Last loop
         if Intrinsic_Functions_Table.Table (I).Name.all = Name
           and then Intrinsic_Functions_Table.Table (I).Width = Width
         then
            return Intrinsic_Functions_Table.Table (I).Func;
         end if;
      end loop;

      case Kind is
         when Unary =>
            Fun_Ty := Fn_Ty ((1 => LLVM_Typ), LLVM_Typ);

         when Binary =>
            Fun_Ty := Fn_Ty ((1 => LLVM_Typ, 2 => LLVM_Typ), LLVM_Typ);

         when Overflow =>
            Fun_Ty := Fn_Ty
              ((1 => LLVM_Typ, 2 => LLVM_Typ),
               Build_Struct_Type ((1 => LLVM_Typ, 2 => Int_Ty (1))));

         when Memcpy =>
            Return_TE := Standard_Void_Type;
            Fun_Ty := Fn_Ty
              ((1 => Void_Ptr_Type, 2 => Void_Ptr_Type,
                3 => Env.LLVM_Size_Type, 4 => Int_Ty (32), 5 => Int_Ty (1)),
               Void_Type);

         when Memset =>
            Return_TE := Standard_Void_Type;
            Fun_Ty := Fn_Ty
              ((1 => Void_Ptr_Type, 2 => Int_Ty (8), 3 => Env.LLVM_Size_Type,
                4 => Int_Ty (32), 5 => Int_Ty (1)),
               Void_Type);
      end case;

      Result := Add_Function (Full_Name, Fun_Ty, Return_TE);
      Intrinsic_Functions_Table.Append ((new String'(Name), Width, Result));
      return Result;
   end Build_Intrinsic;

   -------------------
   -- Emit_One_Body --
   -------------------

   procedure Emit_One_Body (Node : Node_Id) is
      Spec       : constant Node_Id := Get_Acting_Spec (Node);
      Func       : constant GL_Value := Emit_Subprogram_Decl (Spec);
      Def_Ident  : constant Entity_Id := Defining_Entity (Spec);
      Return_Typ : constant Entity_Id := Full_Etype (Def_Ident);
      Param      : Entity_Id;
      LLVM_Param : GL_Value;
      Param_Num  : Natural := 0;

   begin
      Enter_Subp (Func);
      Push_Debug_Scope
        (Create_Subprogram_Debug_Info (Func, Def_Ident, Node,
                                       Get_Name_String (Chars (Def_Ident)),
                                       Get_Ext_Name (Def_Ident)));

      Param := First_Formal_With_Extras (Def_Ident);
      while Present (Param) loop
         LLVM_Param := G (Get_Param (LLVM_Value (Func), unsigned (Param_Num)),
                          Full_Etype (Param),
                          Is_Reference => Param_Needs_Ptr (Param));

         --  Define a name for the parameter Param (which is the
         --  Param_Num'th parameter), and associate the corresponding
         --  LLVM value to its entity.

         Set_Value_Name (LLVM_Value (LLVM_Param), Get_Name (Param));

         --  Special case for structures passed by value, we want to
         --  store a pointer to them on the stack, so do an alloca,
         --  to be able to do GEP on them.

         --  Add the parameter to the environnment

         Set_Value (Param, LLVM_Param);

         if Ekind (Param) = E_In_Parameter
           and then Is_Activation_Record (Param)
         then
            Env.Activation_Rec_Param := LLVM_Param;
         end if;

         Param_Num := Param_Num + 1;
         Param := Next_Formal_With_Extras (Param);
      end loop;

      --  If the return type has dynamic size, we've added a parameter
      --  that's passed the address to which we want to copy our return
      --  value.

      if Ekind (Return_Typ) /= E_Void
        and then Is_Dynamic_Size (Return_Typ)
      then
         LLVM_Param := G (Get_Param (LLVM_Value (Func), unsigned (Param_Num)),
                          Return_Typ, Is_Reference => True);
         Set_Value_Name (LLVM_Value (LLVM_Param), "return");
         Env.Return_Address_Param := LLVM_Param;
      end if;

      Emit_List (Declarations (Node));
      Emit_List (Statements (Handled_Statement_Sequence (Node)));

      --  This point should not be reached: a return must have
      --  already... returned!

      Build_Unreachable;
      Pop_Debug_Scope;
      Leave_Subp;
   end Emit_One_Body;

   --------------------------
   -- Emit_Subprogram_Body --
   --------------------------

   procedure Emit_Subprogram_Body (Node : Node_Id) is
      Nest_Table_First : constant Nat := Nested_Functions_Table.Last + 1;

   begin
      --  If we're not at library level, this a nested function.  Defer it
      --  until we complete elaboration of the enclosing function.  But do
      --  ensure that the spec has been elaborated.

      if not Library_Level then
         Discard (Emit_Subprogram_Decl (Get_Acting_Spec (Node)));
         Nested_Functions_Table.Append (Node);
         return;
      end if;

      --  Otherwise, elaborate this function and then any nested functions
      --  within in.

      Emit_One_Body (Node);

      for I in Nest_Table_First .. Nested_Functions_Table.Last loop
         Emit_Subprogram_Body (Nested_Functions_Table.Table (I));
      end loop;

      Nested_Functions_Table.Set_Last (Nest_Table_First);
   end Emit_Subprogram_Body;

   ---------------------
   -- Get_Static_Link --
   ---------------------

   function Get_Static_Link (Node : Entity_Id) return GL_Value
   is
      Subp        : constant Entity_Id := Entity (Node);
      Result      : GL_Value;
      Parent : constant Entity_Id := Enclosing_Subprogram (Subp);
      Caller : Node_Id;

   begin
      --  ?? This routine needs a major rewrite

      if Present (Parent) then
         Caller := Node_Enclosing_Subprogram (Node);

         declare
            Ent        : constant Subp_Entry :=
              Subps.Table (Subp_Index (Parent));
            Ent_Caller : constant Subp_Entry :=
              Subps.Table (Subp_Index (Caller));

         begin
            if Parent = Caller then
               Result := Need_Value (Get_Value (Ent.ARECnP), Ent.ARECnPT);
            else
               Result := Get_Value (Ent_Caller.ARECnF);

               --  Go levels up via the ARECnU field if needed

               for J in 1 .. Ent_Caller.Lev - Ent.Lev - 1 loop
                  Result := Load (GEP (Env.Size_Type, Result,
                                       (1 => Const_Null_32,
                                        2 => Const_Null_32),
                                       "ARECnF.all.ARECnU"));
               end loop;
            end if;

            return Pointer_Cast (Result, Standard_A_Char, "static-link");
         end;
      else
         return Const_Null (Standard_A_Char);
      end if;
   end Get_Static_Link;

   -------------------
   -- Emit_LCH_Call --
   -------------------

   procedure Emit_LCH_Call (Node : Node_Id) is
      Void_Ptr_Type : constant Type_T := Pointer_Type (Int_Ty (8), 0);
      Int_Type      : constant Type_T := Create_Type (Standard_Integer);
      Args          : Value_Array (1 .. 2);

      File : constant String :=
        Get_Name_String (Reference_Name (Get_Source_File_Index (Sloc (Node))));

      Element_Type : constant Type_T :=
        Int_Type_In_Context (Env.Ctx, 8);
      Array_Type   : constant Type_T :=
        LLVM.Core.Array_Type (Element_Type, File'Length + 1);
      Elements     : array (1 .. File'Length + 1) of Value_T;
      V            : constant Value_T :=
                       Add_Global (Env.Mdl, Array_Type, "str-lit");

   begin
      --  Build a call to __gnat_last_chance_handler (FILE, LINE)

      --  First build a string literal for FILE

      for J in File'Range loop
         Elements (J) := Const_Int
           (Element_Type,
            unsigned_long_long (Character'Pos (File (J))),
            Sign_Extend => False);
      end loop;

      --  Append NUL character

      Elements (Elements'Last) :=
        Const_Int (Element_Type, 0, Sign_Extend => False);

      Set_Initializer
        (V, Const_Array (Element_Type, Elements'Address, Elements'Length));
      Set_Linkage (V, Private_Linkage);
      Set_Global_Constant (V, True);

      Args (1) := Bit_Cast
        (Env.Bld,
         GEP
           (Env.Bld,
            V,
            (Const_Int (Env.LLVM_Size_Type, 0, Sign_Extend => False),
             Const_Int (Create_Type (Standard_Positive),
                        0, Sign_Extend => False)),
            ""),
         Void_Ptr_Type,
         "");

      --  Then provide the line number

      Args (2) := Const_Int
        (Int_Type,
         unsigned_long_long (Get_Logical_Line_Number (Sloc (Node))),
         Sign_Extend => False);
      Discard (Call (Env.Bld, Env.LCH_Fn, Args'Address, Args'Length, ""));
   end Emit_LCH_Call;

   ---------------
   -- Emit_Call --
   ---------------

   function Emit_Call (Call_Node : Node_Id) return GL_Value is
      Subp           : Node_Id := Name (Call_Node);
      Return_Typ     : constant Entity_Id := Full_Etype (Call_Node);
      Void_Return    : constant Boolean := Ekind (Return_Typ) = E_Void;
      Dynamic_Return : constant Boolean :=
        not Void_Return and then Is_Dynamic_Size (Return_Typ);
      Direct_Call    : constant Boolean :=
        Nkind (Subp) /= N_Explicit_Dereference;
      Subp_Typ       : constant Entity_Id :=
        (if Direct_Call then Entity (Subp) else Full_Etype (Subp));
      Param          : Node_Id;
      P_Type         : Entity_Id;
      Actual         : Node_Id;

      --  If it's not an identifier, it must be an access to a subprogram and
      --  in such a case, it must accept a static link.

      This_Takes_S_Link : constant Boolean := not Direct_Call
          and then Needs_Activation_Record (Subp_Typ);
      S_Link         : GL_Value;
      LLVM_Func      : GL_Value;
      Orig_Arg_Count : constant Nat := Count_Params (Subp_Typ);
      Args_Count     : constant Nat :=
        Orig_Arg_Count + (if This_Takes_S_Link then 1 else 0) +
          (if Dynamic_Return then 1 else 0);
      Args           : GL_Value_Array (1 .. Args_Count);
      Idx            : Nat := 1;

   begin

      if Direct_Call then
         Subp := Entity (Subp);
      end if;

      LLVM_Func := Emit_Expression (Subp);
      if This_Takes_S_Link then
         S_Link := Extract_Value (Standard_A_Char,
                                  LLVM_Func, 1, "static-link");
         LLVM_Func := Ptr_To_Ref (Extract_Value
                                    (Standard_A_Char,
                                     LLVM_Func, 0, "callback"),
                                  Full_Designated_Type
                                    (Full_Etype (Prefix (Subp))));
      end if;

      Param  := First_Formal_With_Extras (Subp_Typ);
      Actual := First_Actual (Call_Node);
      while Present (Actual) loop
         P_Type := Full_Etype (Param);

         --  We have two cases: if the param isn't passed indirectly, convert
         --  the value to the parameter's type.  If it is, then convert the
         --  pointer to being a pointer to the parameter's type.

         if Param_Needs_Ptr (Param) then
            Args (Idx) := Convert_To_Access_To (Emit_LValue (Actual), P_Type);
         else
            Args (Idx) := Build_Type_Conversion (P_Type, Actual);
         end if;

         Idx := Idx + 1;
         Actual := Next_Actual (Actual);
         Param := Next_Formal_With_Extras (Param);
         pragma Assert (No (Actual) = No (Param));
      end loop;

      --  Set the argument for the static link, if any

      if This_Takes_S_Link then
         Args (Orig_Arg_Count + 1) := S_Link;
      end if;

      --  Add a pointer to the location of the return value if the return
      --  type is of dynamic size.

      if Dynamic_Return then
         Args (Args'Last) :=
           Allocate_For_Type (Return_Typ, "call-return");
      end if;

      --  If the return type is of dynamic size, call as a procedure and
      --  return the address we set as the last parameter.

      if Dynamic_Return then
         Discard (Call (LLVM_Func, Standard_Void_Type, Args));
         return Args (Args'Last);
      else
         return Call (LLVM_Func, Return_Typ, Args);
      end if;
   end Emit_Call;

   --------------------------
   -- Emit_Subprogram_Decl --
   --------------------------

   function Emit_Subprogram_Decl (Subp_Spec : Node_Id) return GL_Value
   is
      Def_Ident : constant Node_Id := Defining_Entity (Subp_Spec);

   begin
      --  If this subprogram specification has already been compiled, do
      --  nothing.

      if Has_Value (Def_Ident) then
         return Get_Value (Def_Ident);
      else
         declare
            Subp_Type : constant Type_T :=
              Create_Subprogram_Type_From_Spec (Subp_Spec);

            Subp_Base_Name : constant String := Get_Ext_Name (Def_Ident);
            LLVM_Func      : GL_Value;

         begin
            --  ??? Special case __gnat_last_chance_handler which is
            --  already defined as Env.LCH_Fn

            if Subp_Base_Name = "__gnat_last_chance_handler" then
               return G (Env.LCH_Fn, Standard_Void_Type,
                         Is_Reference => True, Is_Subprogram_Type => True);
            end if;

            --  ?? We have a tricky issue here.  We need to indicate that this
            --  object is a subprogram, but we don't have a GNAT type
            --  corresponding to the subprogram type unless there's an access
            --  type.  So we'll use the the return type and flag it as
            --  both a reference and intermediate type.  There doesn't seem
            --  to be better way to handle this right now.

            LLVM_Func :=
              G (Add_Function
                   (Env.Mdl,
                    (if Is_Compilation_Unit (Def_Ident)
                       then "_ada_" & Subp_Base_Name
                       else Subp_Base_Name),
                         Subp_Type),
                 Full_Etype (Def_Ident),
                 Is_Reference => True, Is_Subprogram_Type => True);
            --  Define the appropriate linkage

            if not Is_Public (Def_Ident) then
               Set_Linkage (LLVM_Value (LLVM_Func), Internal_Linkage);
            end if;

            Set_Value (Def_Ident, LLVM_Func);
            return LLVM_Func;
         end;
      end if;
   end Emit_Subprogram_Decl;

   -------------------------------
   -- Node_Enclosing_Subprogram --
   -------------------------------

   function Node_Enclosing_Subprogram (Node : Node_Id) return Node_Id is
      N : Node_Id := Node;

   begin
      while Present (N) loop
         if Nkind (N) = N_Subprogram_Body then
            N := Defining_Unit_Name (Specification (N));
            if Nkind (N) = N_Defining_Program_Unit_Name then
               N := Defining_Identifier (N);
            end if;

            return N;
         end if;

         N := Parent (N);
      end loop;

      return N;
   end Node_Enclosing_Subprogram;

   --------------
   -- Subp_Ptr --
   --------------

   function Subp_Ptr (Node : Node_Id) return GL_Value is
   begin
      if Nkind (Node) = N_Null then
         return Const_Null (Standard_A_Char);
      else
         return Load
           (GEP (Standard_A_Char, Emit_LValue (Node),
                 (1 => Const_Null_32, 2 => Const_Null_32)));
      end if;
   end Subp_Ptr;

end GNATLLVM.Subprograms;
