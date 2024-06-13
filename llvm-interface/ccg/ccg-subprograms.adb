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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Hashed_Maps;

with Interfaces.C; use Interfaces.C;

with Atree;  use Atree;
with Lib;    use Lib;
with Output; use Output;
with Table;

with GNATLLVM.Codegen;     use GNATLLVM.Codegen;
with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.GLValue;     use GNATLLVM.GLValue;
with GNATLLVM.Utils;       use GNATLLVM.Utils;
with GNATLLVM.Wrapper;     use GNATLLVM.Wrapper;

with CCG.Aggregates;   use CCG.Aggregates;
with CCG.Codegen;      use CCG.Codegen;
with CCG.Environment;  use CCG.Environment;
with CCG.Flow;         use CCG.Flow;
with CCG.Instructions; use CCG.Instructions;
with CCG.Output;       use CCG.Output;
with CCG.Strs;         use CCG.Strs;
with CCG.Target;       use CCG.Target;
with CCG.Transform;    use CCG.Transform;
with CCG.Write;        use CCG.Write;

use CCG.Value_Sets;

package body CCG.Subprograms is

   Subprogram_Idx_Low_Bound  : constant := 900_000_000;
   Subprogram_Idx_High_Bound : constant := 999_999_999;
   type Subprogram_Idx is
     range Subprogram_Idx_Low_Bound .. Subprogram_Idx_High_Bound;
   Subprogram_Idx_Start      : constant Subprogram_Idx :=
     Subprogram_Idx_Low_Bound + 1;
   Empty_Subprogram_Idx      : constant Subprogram_Idx :=
     Subprogram_Idx_Low_Bound;

   function Present (Idx : Subprogram_Idx) return Boolean is
     (Idx /= Empty_Subprogram_Idx);
   function No      (Idx : Subprogram_Idx) return Boolean is
     (Idx = Empty_Subprogram_Idx);

   --  For each subprogram, we record the first and last decl and statement
   --  belonging to that subprogram.

   type Subprogram_Data is record
      Func       : Value_T;
      First_Decl : Local_Decl_Idx;
      Last_Decl  : Local_Decl_Idx;
      First_Stmt : Stmt_Idx;
      Last_Stmt  : Stmt_Idx;
   end record;

   package Subprograms is new Table.Table
     (Table_Component_Type => Subprogram_Data,
      Table_Index_Type     => Subprogram_Idx,
      Table_Low_Bound      => Subprogram_Idx_Low_Bound,
      Table_Initial        => 50,
      Table_Increment      => 50,
      Table_Name           => "Subprograms");

   --  We track the order in which file-level variables and subprograms
   --  are declared, along with pragmas Comment and Annotate. We use this
   --  to decide what order to output our translations.

   type Source_Order_Idx is new Nat;
   package Source_Order is new Table.Table
     (Table_Component_Type => Node_Id,
      Table_Index_Type     => Source_Order_Idx,
      Table_Low_Bound      => 1,
      Table_Initial        => 50,
      Table_Increment      => 50,
      Table_Name           => "Source_Order");

   procedure Delete_From_Source_Order (V : Value_T) with Convention => C;
   --  Called when V, which we know has been added to the source order
   --  table, is deleted. Remove it from the table if so.

   function Referenced_Value
     (J : Source_Order_Idx; Defining : out Boolean) return Value_T;
   --  Find the value, if any, being referenced (declared or defined) in
   --  the subtree rooted at entry J in the source order table, if any.
   --  Defining says if it's being declared or defined there.

   function Function_Proto
     (V           : Value_T;
      Definition  : Boolean := True;
      Need_Extern : Boolean := True) return Str
     with Pre  => Is_A_Function (V),
          Post => Present (Function_Proto'Result);
   --  Return the prototype for function V, for either a definition or
   --  a declaration, as appropriate. If Need_Extern is True, we must
   --  always write "extern".

   function Is_Builtin_Name (S : String) return Boolean is
     (S'Length > 5 and then S (S'First .. S'First + 4) = "llvm.");
   --  Return True if S denotes an LLVM builtin function

   function Call_Builtin
     (V : Value_T; S : String; Ops : Value_Array) return Boolean
      with Pre => Get_Opcode (V) = Op_Call;
   --  Call V, a call to a builtin function whose name is S, with operands
   --  of Ops. Return True if the builtin is handled and supported, False
   --  otherwise.

   function Effective_Return_Type
     (T : Type_T; V : Value_T := No_Value_T) return Str
     with Pre  => Is_Function_Type (T),
          Post => Present (Effective_Return_Type'Result);
   --  Return a string corresponding to the return type of T, adjusting the
   --  type in the case where it's an array. If V is Present, it's a value
   --  representing the function so we can get the signedness of the type.

   function Matches
     (S, Name : String; Exact : Boolean := False) return Boolean
   is
     (S'Length >= Name'Length + 5
        and then (not Exact or else S'Length = Name'Length + 5)
        and then S (S'First + 5 .. S'First + Name'Length + 4) = Name);
   --  True iff the string Name is present starting at position 5 of S
   --  (after "llvm."). If Exact is true, there must be nothing following
   --  Name in S.

   type Arithmetic_Operation is (Add, Subtract, Multiply);

   function Op_With_Overflow
     (V    : Value_T;
      Ops  : Value_Array;
      S    : String;
      Arit : Arithmetic_Operation) return Boolean
     with Pre => Present (V) and then Ops'Length >= 2;
   --  Handle an arithmetic operation with overflow. Return True if we're able
   --  to process this builtin.

   Overflow_Declared : array (Arithmetic_Operation) of Boolean :=
     (others => False);

   function Memory_Operation
     (V : Value_T; Ops : Value_Array; S : String) return Boolean
     with Pre => Present (V) and then Ops'Length >= 3;
   --  Process memcpy, memmove, and memset. Return True if we're able to
   --  process this builtin.

   function Funnel_Shift
     (V : Value_T; Ops : Value_Array; Left : Boolean) return Boolean
     with Pre => Present (V) and then Ops'Length >= 3;
   --  Process a left or right funnel shift builtin. Return True if we're
   --  able to process this builtin.

   function Bit_Byte_Reverse
     (V : Value_T; Op : Value_T; Is_Byte : Boolean) return Str
     with Pre => Present (V) and then Present (Op);
   --  Process a bitreverse or byte swap builtin

   ------------------------------
   -- Delete_From_Source_Order --
   ------------------------------

   procedure Delete_From_Source_Order (V : Value_T) is
      Defining : Boolean;

   begin
      --  Scan the source order table to find the entry for V and invalidate
      --  that entry. This is quadratic on the number of deleted values,
      --  but deleting values that are referenced in the source is
      --  quite rare.

      for J in 1 .. Source_Order.Last loop
         if Referenced_Value (J, Defining) = V then
            Source_Order.Set_Item (J, Types.Empty);
            exit;
         end if;
      end loop;
   end Delete_From_Source_Order;

   ----------------------
   -- Referenced_Value --
   ----------------------

   function Referenced_Value
     (J : Source_Order_Idx; Defining : out Boolean) return Value_T
   is
      N : constant Node_Id := Source_Order.Table (J);
      E : Entity_Id        := Types.Empty;

   begin
      --  If there's no entity (in the rare case where a value was deleted),
      --  show there's no value either.

      if No (N) then
         return No_Value_T;
      end if;

      --  In most cases, we're just declaring the value

      Defining := False;

      case Nkind (N) is
         when N_Object_Declaration | N_Object_Renaming_Declaration |
           N_Exception_Declaration | N_Exception_Renaming_Declaration
           =>
            E := Defining_Identifier (N);

         when N_Subprogram_Declaration =>
            E := Defining_Unit_Name (Specification (N));

         when N_Subprogram_Body =>
            Defining := True;
            E        := Defining_Unit_Name (Acting_Spec (N));

         when N_Pragma =>
            null;

         when others =>
            pragma Assert (Standard.False);
      end case;

      --  If the entity has a value, return it

      return
        (if   Present (E) and then Present (Get_Value (E))
           then +Get_Value (E) else No_Value_T);
   end Referenced_Value;

   -------------------------
   -- Add_To_Source_Order --
   -------------------------

   procedure Add_To_Source_Order (N : Node_Id) is
   begin
      --  If this is an expression function, treat it as if it's from source

      if Comes_From_Source (N)
        or else (Nkind (N) = N_Subprogram_Body
                   and then Was_Expression_Function (N))
      then
         Source_Order.Append (N);
      end if;
   end Add_To_Source_Order;

   --------------------------
   -- Protect_Source_Order --
   --------------------------

   procedure Protect_Source_Order is
      Defining : Boolean;

   begin
      --  Note that we can't do this when we add each source order entry
      --  because we may not have made a value for it (e.g., if there's
      --  a freeze node.

      for J in 1 .. Source_Order.Last loop
         Notify_On_Value_Delete
           (Referenced_Value (J, Defining), Delete_From_Source_Order'Access);
      end loop;
   end Protect_Source_Order;

   --------------------
   -- New_Subprogram --
   --------------------

   procedure New_Subprogram (V : Value_T) is
   begin
      Subprograms.Append ((Func       => V,
                           First_Decl => Empty_Local_Decl_Idx,
                           Last_Decl  => Empty_Local_Decl_Idx,
                           First_Stmt => Empty_Stmt_Idx,
                           Last_Stmt  => Empty_Stmt_Idx));
   end New_Subprogram;

   ---------------
   -- Curr_Func --
   ---------------

   function Curr_Func return Value_T is
     (Subprograms.Table (Subprograms.Last).Func);

   ----------------------------------
   -- Output_Function_Type_Typedef --
   ----------------------------------

   procedure Output_Function_Type_Typedef (T : Type_T) is
      Fn_T       : constant Type_T := Get_Element_Type (T);
      Num_Params : constant Nat    := Count_Param_Types (Fn_T);
      First      : Boolean         := True;
      Result     : Str             :=
        Effective_Return_Type (Fn_T) & " " & "(*" & T & ")" & " (";
      P_Types    : Type_Array (1 .. Num_Params);

   begin
      if Num_Params = 0 then
         Result := Result & "void";
      else
         Get_Param_Types (Fn_T, P_Types'Address);
         for P_T of P_Types loop
            begin
               Result := Result & (if First then "" else ", ") & P_T;
               First := False;
            end;
         end loop;
      end if;

      Output_Decl ("typedef " & Result & ")", Is_Typedef => True);
   end Output_Function_Type_Typedef;

   ---------------------------
   -- Effective_Return_Type --
   ---------------------------

   function Effective_Return_Type
     (T : Type_T; V : Value_T := No_Value_T) return Str
   is
      Ret_Typ : constant Type_T := Get_Return_Type (T);

   begin
      --  If this function returns an array, change it to return a
      --  struct containing that array.

      if Is_Array_Type (Ret_Typ) then
         Maybe_Output_Array_Return_Typedef (Ret_Typ);
         return Ret_Typ & "_R";
      else
         return Ret_Typ + V;
      end if;
   end Effective_Return_Type;

   --------------------
   -- Function_Proto --
   --------------------

   function Function_Proto
     (V           : Value_T;
      Definition  : Boolean := True;
      Need_Extern : Boolean := True) return Str
   is
      function Has_Unreachable return Boolean;
      --  True if there's an "unreachable" instruction in V

      function In_Main_Unit return Boolean;

      Num_Params     : constant Nat     := Count_Params (V);
      Fn_Typ         : constant Type_T  := Get_Element_Type (V);
      Write_Extern   : Boolean          := Need_Extern;
      Result         : Str              :=
        Effective_Return_Type (Fn_Typ, V) & " " & V & " (";
      Maybe_Add_Nest : constant Boolean :=
        Get_Needs_Nest (V)
        and then (Num_Params = 0
                  or else not Has_Nest_Attribute
                                (V, unsigned (Num_Params - 1)));

      ---------------------
      -- Has_Unreachable --
      ---------------------

      function Has_Unreachable return Boolean is
         BB : Basic_Block_T := Get_First_Basic_Block (V);

      begin
         while Present (BB) loop
            declare
               Term : constant Value_T := Get_Basic_Block_Terminator (BB);
               Prev : constant Value_T := Get_Previous_Instruction (Term);

            begin
               --  An "unreachable" after a call to a noreturn function can
               --  be ignored

               if Get_Opcode (Term) = Op_Unreachable then
                  if No (Prev) or else Get_Opcode (Prev) /= Op_Call then
                     return True;
                  else
                     declare
                        F : constant Value_T :=
                          Get_Operand (Prev, Get_Num_Operands (Prev) - 1);

                     begin
                        if not Is_A_Function (F)
                          or else not Does_Not_Return (F)
                        then
                           return True;
                        end if;
                     end;
                  end if;
               end if;
            end;

            BB := Get_Next_Basic_Block (BB);
         end loop;

         return False;
      end Has_Unreachable;

      ------------------
      -- In_Main_Unit --
      ------------------

      function In_Main_Unit return Boolean is
         E : constant Entity_Id := Get_Entity (V);

      begin
         return Present (E) and then Ekind (E) in Subprogram_Kind
           and then Entity_Is_In_Main_Unit (E);
      end In_Main_Unit;

   begin
      --  If this is an internal subprogram, mark it as static

      if Get_Linkage (V) = Internal_Linkage then
         Result := "static " & Result;
      end if;

      --  If this has a linker section, indicate that

      if Get_Section (V) /= "" then
         Result :=
           Output_Modifier (Code_Section_Modifier.all, S => Get_Section (V)) &
           Result;
      end if;

      --  If inline was requested and we have the body, mark that, but only
      --  if the language version is recent enough to support it. If it's
      --  not internal and is in the main unit, declare it as "extern".

      if (Has_Inline_Attribute (V) or else Has_Inline_Always_Attribute (V))
        and then C_Version > 1990
        and then Present (Get_First_Basic_Block (V))
      then
         Result := "inline " & Result;
         if (In_Main_Unit or else not Use_FE_Data) and then not Definition
           and then Get_Linkage (V) /= Internal_Linkage
         then
            Write_Extern := True;
         end if;
      end if;

      --  If inline always was requested, mark it as such, but only in
      --  the definition.

      if Has_Inline_Always_Attribute (V) and then C_Version > 1990
        and then Definition
      then
         Result := Output_Modifier ("always_inline") & Result;
      end if;

      --  If this doesn't return mark that. But if we have an "unreachable",
      --  that will confuse the analysis.

      if Does_Not_Return (V) and then not Has_Unreachable then
         Result := Output_Modifier ("noreturn") & Result;
      end if;

      --  Then output the list of parameter types, if any. If this isn't
      --  for an extern definition, include the parameter names.
      --  Special-case calloc since we can't tell that the integral type
      --  is really size_t and that our result is really void *.

      if Num_Params = 0 then
         Result := Result & (if   Maybe_Add_Nest then Generic_Ptr & "_n"
                             else +"void");
      elsif Get_Value_Name (V) = "calloc" then
         Result := +"void *calloc (size_t, size_t";
      else
         for J in 0 .. Num_Params - 1 loop
            declare
               Param  : constant Value_T          := Get_Param (V, J);
               E      : constant Entity_Id        :=
                 Get_Parameter_Entity (V, J);
               BT     : constant Opt_Type_Kind_Id :=
                 Opt_Full_Base_Type (Opt_Full_Etype (E));
               Typ    : Str                       := Type_Of (Param) or E;

            begin
               --  We may be passing an unsigned integer type by reference.
               --  If so, use unsigned version of type.

               if Present (BT) and then Is_Unsigned_Type (BT)
                 and then Is_Pointer_Type (Param)
               then
                  Typ := Type_Of (Param) + Need_Unsigned;
               end if;

               --  Add this parameter to the list, usually preceeded by a comma

               Result := Result & (if J = 0 then "" else ", ") & Typ;
               if Definition then
                  Result := Result & " " & Param;
                  Set_Is_Decl_Output (Param);
               end if;
            end;
         end loop;

         --  If we've determined that we need to add a nest parameter to this
         --  function, do it.

         if Maybe_Add_Nest then
            Result := Result & ", " & Generic_Ptr & " _n";
         end if;
      end if;

      --  If we are to unconditionally write "extern", do it

      if Write_Extern then
         Result := "extern " & Result;
      end if;

      return Result & ")";
   end Function_Proto;

   ------------------------
   -- Declare_Subprogram --
   ------------------------

   procedure Declare_Subprogram (V : Value_T) is
      S : constant String := Get_Value_Name (V);

   begin
      --  Ignore LLVM builtin functions. The most we can have is a declaration
      --  but we don't want them to be emitted: we either handle or don't
      --  handle the builtin, but will never actually call it. We also don't
      --  emit declarations for C functions that are defined in string.h or
      --  stdlib.h.
      --  ??? Exactly how to get that list is far from clear, but let's
      --  approximate for now.

      if Is_Builtin_Name (S)
        or else S = "memcpy" or else S = "memmove" or else S = "memset"
        or else S = "memcmp" or else S = "malloc" or else S = "free"
      then
         return;
      end if;

      --  Otherwise, write the definition of this function. If it has no
      --  basic blocks, it must be an extern. We don't want to use the
      --  debug info here if we'll later have declaration.

      Output_Decl (Function_Proto (V,
                                   Definition  => False,
                                   Need_Extern => Emit_Header
                                     or else No (Get_First_Basic_Block (V))),
                   V             => V,
                   Is_Global     => True,
                   No_Debug_Info => Present (Get_First_Basic_Block (V)));
   end Declare_Subprogram;

   -----------------------
   -- Output_Subprogram --
   -----------------------

   procedure Output_Subprogram (V : Value_T) is
      Idx : Flow_Idx;

   begin
      --  If there are no blocks, we have nothing to do

      if No (Get_First_Basic_Block (V)) then
         return;
      end if;

      --  Otherwise, start a new function, do any transformation to help
      --  our output, make all flows in this subprogram, and ouput each flow.
      --  If we're writing a header file, this must be an inline_always
      --  function, so mark it extern.

      New_Subprogram (V);
      Transform_Blocks (V);
      Output_Decl
        (Function_Proto (V, Need_Extern => Emit_Header),
         Semicolon => False,
         V         => V);
      Output_Decl ("{", Semicolon => False, Start_Block => Decl);
      Idx := Get_Or_Create_Flow (Get_Entry_Basic_Block (V));
      Add_Use (Idx);
      Maybe_Dump_Flow (Idx, V, "Initial");
      Simplify_Flow (Idx);
      Maybe_Dump_Flow (Idx, V, "Simplified");
      Output_Flow (Idx);
      Clear_Pending_Values;

   end Output_Subprogram;

   ----------------------
   -- Call_Instruction --
   ----------------------

   procedure Call_Instruction (V : Value_T; Ops : Value_Array) is

      --  The operands to a call instruction are the parameters of the
      --  function being called followed by the function to call.

      Num_Params  : constant unsigned      := unsigned (Ops'Length - 1);
      Func        : Value_T                := Ops (Ops'Last);
      S           : constant String        := Get_Value_Name (Func);
      First       : Boolean                := True;
      Cast_T      : Type_Array (Ops'Range) := (others => No_Type_T);
      Call        : Str;

   begin
      --  If this is an undef, don't do anything

      if Is_Undef (Func) then
         return;

      --  If this is a builtin, handle that

      elsif Is_Builtin_Name (S) then
         if Call_Builtin (V, S, Ops) then
            return;
         end if;

         --  By default generate a call to the builtin with a C-compatible
         --  name, to let users the chance to provide a suitable
         --  implementation.

         declare
            Builtin : String (S'Range);
         begin
            for J in S'Range loop
               Builtin (J) := (if S (J) = '.' then '_' else S (J));
            end loop;

            Error_Msg ("unsupported builtin: " & Builtin, V);
            Call :=
              "/* unsupported builtin */ " & (Builtin + Component) & " (";
         end;

      --  Otherwise, see if our function is a bitcast from one
      --  function type to another and the two are compatible.
      --  We don't do this if the return types aren't the same and are
      --  structure types because that'll require copying.

      elsif Is_A_Constant_Expr (Func) and then Get_Opcode (Func) = Op_Bit_Cast
      then
         declare
            Src_T     : constant Type_T := Type_Of (Func);
            Dest_T    : constant Type_T := Type_Of (Get_Operand0 (Func));
            Dest_Fn_T : constant Type_T := Get_Element_Type (Dest_T);
            Src_Fn_T  : constant Type_T := Get_Element_Type (Src_T);

         begin
            if Is_Pointer_Type (Dest_T)
              and then Is_Function_Type (Dest_Fn_T)
              and then Is_Layout_Identical (Src_Fn_T, Dest_Fn_T)
              and then Count_Param_Types (Dest_Fn_T) = Num_Params
              and then (Get_Return_Type (Dest_Fn_T) =
                        Get_Return_Type (Src_Fn_T)
                        or else not Is_Struct_Type (Get_Return_Type
                                                      (Src_Fn_T)))
            then
               declare
                  Src_P_Types  : Type_Array (Ops'Range);
                  Dest_P_Types : Type_Array (Ops'Range);

               begin
                  --  Mark which parameters have a different type (though
                  --  an identical layout).

                  Get_Param_Types (Src_Fn_T, Src_P_Types'Address);
                  Get_Param_Types (Dest_Fn_T, Dest_P_Types'Address);
                  for J in Ops'First .. Ops'Last - 1 loop
                     if Src_P_Types (J) /= Dest_P_Types (J) then
                        Cast_T (J) := Dest_P_Types (J);
                     end if;
                  end loop;

                  --  See if the return needs a cast and then get the new
                  --  function to call.

                  if Get_Return_Type (Src_Fn_T) /= Get_Return_Type (Dest_Fn_T)
                  then
                     Cast_T (Cast_T'Last) := Get_Return_Type (Dest_Fn_T);
                  end if;

                  Func := Get_Operand0 (Func);
               end;
            end if;
         end;
      end if;

      --  Now generate the argument list for the call

      Call := Func + Component & " (";
      for J in Ops'First .. Ops'Last - 1 loop
         declare
            Op    : constant Value_T := Ops (J);
            Param : Str;

         begin
            --  If we need a cast and it's a struct type, we need to force
            --  Op into a variable.

            if Present (Cast_T (J)) and then Is_Struct_Type (Op) then
               Force_To_Variable (Op);
            end if;

            --  If Op is a constant array, we have to cast it to the
            --  non-constant type which is a pointer to the element type.

            if Is_Array_Type (Op) and then Get_Is_Constant (Op) then
               Param := "(" & Get_Element_Type (Op) & " *) " & (Op + Comma);
            else
               Param := Process_Operand (Op, X, Comma) + Comma;
            end if;

            --  If we need a cast for this operand (because LLVM merged
            --  two functions), generate it. But if we're casting to
            --  a struct type, do it by pointer putting.

            if Present (Cast_T (J)) then
               if Is_Struct_Type (Op) then
                  Param :=
                    Deref ("(" & Cast_T (J) & "*) " & Addr_Of (Param));
               else
                  Param := "(" & Cast_T (J) & ") " & Param;
               end if;
            end if;

            if not First then
               Call := Call & ", ";
            end if;

            Call := Call & Param;
            First := False;
         end;
      end loop;

      --  If we have a function that needs a nest parameter and the last
      --  parameter passed here isn't a nest parameter, pass one.

      if Is_A_Function (Func) and then Get_Needs_Nest (Func)
        and then (Num_Params = 0
                  or else not Call_Param_Has_Nest (V, Num_Params - 1))
      then
         Call := Call & (if First then "" else ", ") & NULL_String;
      end if;

      --  If we need to cast the result to a different type, do that

      if Present (Cast_T (Cast_T'Last)) then
         Call := "(" & Cast_T (Cast_T'Last) & ") " & Call;
      end if;

      --  Add the final close paren, then write any pending values (we
      --  do it here to avoid writing out things only used as part of
      --  the parameter calculation. If this is a procedure call,
      --  output it. Otherwise, set this as the value of V.

      Call := (Call & ")") + Component;
      if Get_Type_Kind (V) = Void_Type_Kind then
         Add_Line (Call, V);
      else
         --  If this returns an array, we've changed it to returning a
         --  struct whose field is that array, so we need to do the
         --  dereference. However, we can't simply return the function call
         --  followed by the dereference because if we're going to be using
         --  the entire array, we'll do a memmove and we can't take the
         --  address of the function result. So we have to make a variable,
         --  assign the function result to it, and dereference that. We
         --  don't have a value to use as the variable, so we have to use a
         --  bit of a kludge. Note that we can't call Output_Copy here
         --  since it'll think that this is an array copy and use memmove.

         if Is_Array_Type (V) then
            declare
               Our_Var : constant Str := "ccg_v" & Get_Output_Idx;

            begin
               Output_Decl (TP ("#T1_R ", V) & Our_Var, V => V);
               Add_Line (Our_Var & " = " & Call + Assign, V);
               Call := Our_Var & ".F" + Component;
            end;
         end if;

         Assignment (V, Call);
      end if;
   end Call_Instruction;

   ----------------------
   -- Op_With_Overflow --
   ----------------------

   function Op_With_Overflow
     (V    : Value_T;
      Ops  : Value_Array;
      S    : String;
      Arit : Arithmetic_Operation) return Boolean
   is
      Op1  : constant Value_T  := Ops (Ops'First);
      Op2  : constant Value_T  := Ops (Ops'First + 1);
      Subp : constant String   :=
        "system__arith_64__" & To_Lower (Arit'Image) & "_with_ovflo_check64";
      Bits : constant unsigned := unsigned'Value (S (S'First + 25 .. S'Last));

   begin
      Maybe_Decl (V);

      if not Overflow_Declared (Arit) then
         Output_Decl ("extern long long " & Subp & " (long long, long long)",
                      Is_Global => True);
         Overflow_Declared (Arit) := True;
      end if;

      --  Overflow builtins are only generated by the LLVM optimizer (see
      --  lib/Transforms/InstCombineCompares.cpp), but we still want to
      --  handle them by calling the routines in System.Arith_64 even if
      --  this is clearly inefficient.
      --
      --  The LLVM builtin deals with a struct containing two fields: the
      --  first is the integer result, the second is the overflow bit.  If
      --  the Arith_64 routine succeeds (does not raise an exception), it
      --  means that no overflow occurred so always clear the second field.
      --  ??? The front end is able to convert some overflow operations to
      --  direct comparison. We ought to do the same here. And if we do
      --  that, then we can emit the builtins in all cases and allow the
      --  LLVM optimizer to see the builtins, which should allow it to do a
      --  better job. But this is for later work; we need to get a better
      --  idea of the tradeoffs here.

      Output_Copy (+V & ".ccg_field_0",
                   "(" & Int_Type_String (Pos (Bits), False) & ") " & Subp &
                   " ((long long) " & Op1 & ", (long long) " & Op2 & ")",
                   Int_Type (Bits),
                   V => V);
      Output_Copy (+V & ".ccg_field_1", +"0", Int_Type (1), V => V);
      return True;
   end Op_With_Overflow;

   ------------------
   -- Funnel_Shift --
   ------------------

   function Funnel_Shift
     (V : Value_T; Ops : Value_Array; Left : Boolean) return Boolean
   is
      Op1      : constant Value_T := Ops (Ops'First);
      Op2      : constant Value_T := Ops (Ops'First + 1);
      Op3      : constant Value_T := Ops (Ops'First + 2);
      Str_Op1  : constant Str     :=
        Process_Operand (Op1, POO_Unsigned, Shift);
      Str_Op2  : constant Str     :=
        Process_Operand (Op2, POO_Unsigned, Shift);
      Size     : constant Nat     := Get_Scalar_Bit_Size (Op1);
      Cnt      : Nat;
      Sh1, Sh2 : Str;
      Result   : Str;

   begin
      --  There are two cases here, where Op3 is an integer and where
      --  it isn't. The second is more complex.
      --  ??? We're not going to support the non-variable case until we
      --  see an occurence of it since it'll be hard to debug.

      if not Is_A_Constant_Int (Op3) then
         return False;
      end if;

      --  Otherwise, get the constant shift count and generate the OR of
      --  the two shifts. Then cast it back to our type to truncate or
      --  sign-extend.

      Cnt := Nat (Const_Int_Get_Z_Ext_Value (Op3));
      Sh1 := (Str_Op1 + Shift) & (if Left then " << " else " >> ") & Cnt;
      Sh2 :=
        (Str_Op2 + Shift) & (if Left then " >> " else " << ") & (Size - Cnt);
      Result := "(" & (V + Write_Type) & ") (" & Sh1 + Bit & " | " & Sh2 & ")";
      Assignment (V, Result, Is_Opencode_Builtin => True);
      return True;

   end Funnel_Shift;

   ----------------------
   -- Bit_Byte_Reverse --
   ----------------------

   function Bit_Byte_Reverse
     (V : Value_T; Op : Value_T; Is_Byte : Boolean) return Str
   is
      Width     : constant Nat := Get_Scalar_Bit_Size (Type_Of (V));
      Part_Size : constant Nat := (if Is_Byte then BPU else 1);
      Mask      : constant Nat := 2 ** Integer (Part_Size) - 1;

   begin

      --  We generate Width / Part operations whose results are "or"ed
      --  together. Each can be written as one or more shifts with an
      --  "and".  The simplest way to code this, which avoids having to be
      --  concerned about large constants or signedness, is to shift the part
      --  (bit or byte) to the low-order bit(s), mask it to a bit or byte,
      --  and shift it back.

      return Result : Str := +"" do
         for J in 0 .. Width / Part_Size - 1 loop
            declare
               Initial_Pos : constant Nat := Width - (J + 1) * Part_Size;
               Desired_Pos : constant Nat := J * Part_Size;
               Piece : Str := +Op;

            begin
               --  If this isn't at the low-order location, move it there

               if Initial_Pos /= 0 then
                  Piece := Piece + Shift & " >> " & Initial_Pos;
               end if;

               --  Now do the mask and put it where we want it, if not
               --  already there.

               Piece := Piece + Bit & " & " & Mask;
               if Desired_Pos /= 0 then
                  Piece := Piece + Shift & " << " & Desired_Pos;
               end if;

               --  Finally, add this piece to the result

               Result := Result + Bit & (if J /= 0 then " | " else "") & Piece;
            end;
         end loop;
      end return;
   end Bit_Byte_Reverse;

   -----------------------
   --  Memory_Operation --
   -----------------------

   function Memory_Operation
     (V : Value_T; Ops : Value_Array; S : String) return Boolean
   is
      Op1    : constant Value_T := Ops (Ops'First);
      Op2    : constant Value_T := Ops (Ops'First + 1);
      Op3    : constant Value_T := Ops (Ops'First + 2);
      Result : Str;

   begin
      Result := S & " (" & Op1 & ", " & Op2 & ", " & Op3 & ")";
      Add_Line (Result, V);
      return True;
   end Memory_Operation;

   ------------------
   -- Call_Builtin --
   ------------------

   function Call_Builtin
     (V : Value_T; S : String; Ops : Value_Array) return Boolean
   is
      Op1 : constant Value_T  :=
        (if Ops'Length >= 1 then Ops (Ops'First) else No_Value_T);
      Op2 : constant Value_T  :=
        (if Ops'Length >= 2 then Ops (Ops'First + 1) else No_Value_T);

   begin
      --  We ignore experimental noalias call and llvm.assume. Also ignore
      --  stackrestore/stacksave calls: these are generated by the
      --  optimizer and in many cases the stack usage is actually zero or
      --  very low.
      --  ??? Not clear that we can do better for now.

      if Matches (S, "assume")
        or else Matches (S, "experimental.noalias.scope.decl")
        or else Matches (S, "stackrestore", True)
        or else Matches (S, "stacksave", True)
      then
         return True;

      --  Handle some overflow intrinsics

      elsif Matches (S, "sadd.with.overflow") then
         return Op_With_Overflow (V, Ops, S, Add);
      elsif Matches (S, "ssub.with.overflow") then
         return Op_With_Overflow (V, Ops, S, Subtract);
      elsif Matches (S, "smul.with.overflow") then
         return Op_With_Overflow (V, Ops, S, Multiply);

      --  Handle funnel shifts

      elsif Matches (S, "fshl") then
         return Funnel_Shift (V, Ops, Left => True);
      elsif Matches (S, "fshr") then
         return Funnel_Shift (V, Ops, Left => False);

      --  We process memcpy, memmove, and memset by calling the corresponding
      --  C library function.

      elsif Matches (S, "memcpy") then
         return Memory_Operation (V, Ops, "memcpy");
      elsif Matches (S, "memmove") then
         return Memory_Operation (V, Ops, "memmove");
      elsif Matches (S, "memset") then
         return Memory_Operation (V, Ops, "memset");

      --  For min and max, we have to make sure that the operands are the
      --  right signedness. Even though we're forcing the operands to
      --  variables (to avoid side-effects), the LLVM optimizer may choose
      --  to generate a signed min/max for a variable that's used as unsigned
      --  and vice versa.

      elsif Matches (S, "smax") or else Matches (S, "umax")
        or else Matches (S, "smin") or else Matches (S, "umin")
      then
         declare
            POO    : constant Process_Operand_Option :=
              (if Matches (S, "umin") or else Matches (S, "umax")
               then POO_Unsigned else POO_Signed);
            Is_Max : constant Boolean :=
              Matches (S, "smax") or else Matches (S, "umax");

         begin
            Force_To_Variable (Op1);
            Force_To_Variable (Op2);
            Assignment (V,
                        Process_Operand (Op1, POO, Conditional) &
                        (if Is_Max then " > " else " < ") &
                        Process_Operand (Op2, POO, Conditional) &
                        " ? " & Op1 & " : " & Op2 + Conditional,
                        Is_Opencode_Builtin => True);
            return True;
         end;

      --  abs is simple, but we have to be sure that we do a signed operation

      elsif Matches (S, "abs") then
         Force_To_Variable (Op1);

         declare
            Str1 : constant Str :=
              Process_Operand (Op1, POO_Signed, Conditional);

         begin
            Assignment (V,
                        Str1 & " >= 0 ? " & Str1 & " : - " &
                        Str1 + Conditional,
                        Is_Opencode_Builtin => True);
            return True;
         end;

      --  The only saturating arithmetic we've seen the LLVM optimizer
      --  generate so far is unsigned subtraction, so that's all we support
      --  at the moment. But we have to be sure the operands are the proper
      --  signedness for the comparison.

      elsif Matches (S, "usub.sat") then
         Force_To_Variable (Op1);
         Force_To_Variable (Op2);

         declare
            Str1 : constant Str :=
              Process_Operand (Op1, POO_Unsigned, Conditional);
            Str2 : constant Str :=
              Process_Operand (Op2, POO_Unsigned, Conditional);

         begin
            Assignment (V,
                        Str2 & " > " & Str1 & " ? 0 : " & Str1 & " - " &
                        Str2 + Conditional,
                        Is_Opencode_Builtin => True);
            return True;
         end;

      --  Handle copysign. We could do this by calling the library
      --  function, but that would add complexity to the user's
      --  build. Doing this as a conditional is equivalent except for
      --  signed zeros and obscure cases involving NaNs and it appears that
      --  LLVM doesn't generate this builtin in most of those cases. If
      --  this ever becomes no longer true and it becomes an issue, we may
      --  need to add a switch that forces exact FP semantics and makes
      --  the library call.

      elsif Matches (S, "copysign") then
         Force_To_Variable (Op1);
         Force_To_Variable (Op2);
         Assignment (V, TP ("(#1 >= 0) == (#2 >= 0) ? #1 : - #1", Op1, Op2)
                       + Conditional,
                     Is_Opencode_Builtin => True);
         return True;

      --  Handle the builtins we created for or else / and then

      elsif Matches (S, "ccg.orelse") then
         Assignment (V, TP ("#1 || #2", Op1, Op2) + Logical_OR,
                     Is_Opencode_Builtin => True);
         return True;
      elsif Matches (S, "ccg.andthen") then
         Assignment (V, TP ("#1 && #2", Op1, Op2) + Logical_AND,
                     Is_Opencode_Builtin => True);
         return True;

      --  Handle bitreverse and bswap builtins

      elsif (Matches (S, "bswap") or else Matches (S, "bitreverse"))
        and then Ops'Length = 2 and then Is_Integral_Type (Op1)
      then
         Force_To_Variable (Op1);
         Assignment (V, Bit_Byte_Reverse (V, Op1, Matches (S, "bswap")));
         return True;

      --  Handle the builtin for annotations

      elsif Matches (S, "ccg.annotate") then
         Output_Annotation (Nat (Const_Int_Get_Z_Ext_Value (Ops (Ops'First))),
                            V, Is_Global => False);
         return True;

      --  And we don't process the rest

      else
         return False;
      end if;

   end Call_Builtin;

   -------------------
   -- Add_Decl_Line --
   -------------------

   procedure Add_Decl_Line (Idx : Local_Decl_Idx) is
      SD : Subprogram_Data renames Subprograms.Table (Subprograms.Last);

   begin
      --  If this is the first we've written, set it as the first and last
      --  for the current subprogram. Otherwise, verify that it's consecutive
      --  to the last decl we added to this subprogram.

      if No (SD.First_Decl) then
         SD.First_Decl := Idx;
      else
         pragma Assert (Idx = SD.Last_Decl + 1);
      end if;

      SD.Last_Decl := Idx;
   end Add_Decl_Line;

   -------------------
   -- Add_Stmt_Line --
   -------------------

   procedure Add_Stmt_Line (Idx : Stmt_Idx) is
      SD : Subprogram_Data renames Subprograms.Table (Subprograms.Last);

   begin
      --  If this is the first we've written, set it as the first and last
      --  for the current subprogram. Otherwise, verify that it's consecutive
      --  to the last statement we added to this subprogram.

      if No (SD.First_Stmt) then
         SD.First_Stmt := Idx;
      else
         pragma Assert (Idx = SD.Last_Stmt + 1);
      end if;

      SD.Last_Stmt := Idx;
   end Add_Stmt_Line;

   ------------------
   -- Write_C_File --
   ------------------

   procedure Write_C_File is
      procedure Write_One_Subprogram (Sidx : Subprogram_Idx)
        with Pre => Present (Sidx);
      --  Write the declarations and statements for Sidx, a subprogram

      procedure Write_One_Declaration (Gidx : Global_Decl_Idx; V : Value_T)
        with Pre => Present (Gidx);
      --  Write the declaratation for Gidx

      procedure Maybe_Write_Decl (V : Value_T);
      --  If V is queued to be output later, output it now and remove it
      --  from the list.

      procedure Scan_For_Decls is new Walk_Object (Maybe_Write_Decl);

      --  We have two maps, one mapping a value (either a global variable
      --  or a subprogram) being declared to the (first) global decl that
      --  outputs it and one mapping a value denoting a function to the
      --  subprogram entry for it.

      package Value_To_Decl_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Value_T,
         Element_Type    => Global_Decl_Idx,
         Hash            => Hash,
         Equivalent_Keys => "=");
      package Value_To_Subprogram_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Value_T,
         Element_Type    => Subprogram_Idx,
         Hash            => Hash,
         Equivalent_Keys => "=");
      use Value_To_Decl_Maps;
      use Value_To_Subprogram_Maps;

      Elab_Spec_SI    : Subprogram_Idx := Empty_Subprogram_Idx;
      Elab_Body_SI    : Subprogram_Idx := Empty_Subprogram_Idx;
      Declaration_Map : Value_To_Decl_Maps.Map;
      Definition_Map  : Value_To_Subprogram_Maps.Map;

      --------------------------
      -- Write_One_Subprogram --
      --------------------------

      procedure Write_One_Subprogram (Sidx : Subprogram_Idx) is
         SD : constant Subprogram_Data := Subprograms.Table (Sidx);

      begin
         --  Scan this subprogram for any decls we have to write first

         Scan_For_Decls (SD.Func);

         --  Now write our decls. We at least have the function prototype.

         Write_Eol;
         for Idx in SD.First_Decl .. SD.Last_Decl loop
            Write_C_Line (Idx);
         end loop;

         --  If we're written more than just the prototype and the
         --  "{", add a blank line between the decls and statements.

         if SD.Last_Decl > SD.First_Decl + 1 then
            Write_Eol;
         end if;

         --  Now write out the statements for the subprogram, if any

         if Present (SD.First_Stmt) then
            for Idx in SD.First_Stmt .. SD.Last_Stmt loop
               Write_C_Line (Idx);
            end loop;
         end if;

         --  Finally, write the closing brace. We have to do it this
         --  way rather than using End_Output_Block because we can't
         --  know in what order basic blocks will be written when
         --  we're outputting them.

         Write_C_Line ("}", End_Block => Decl);
         Exclude (Definition_Map, SD.Func);
      end Write_One_Subprogram;

      ---------------------------
      -- Write_One_Declaration --
      ---------------------------

      procedure Write_One_Declaration (Gidx : Global_Decl_Idx; V : Value_T) is
         Our_Gidx : Global_Decl_Idx := Gidx;

      begin
         --  Show that we've written the value corresponding to this
         --  declaration to avoid doing it more than once.

         Exclude (Declaration_Map, V);

         --  First write any decls that this depends on

         Scan_For_Decls (V);

         --  Write any decls that declare this variable. There should
         --  only be one, but we don't want or need to depend on that.

         while Our_Gidx <= Get_Last_Global_Decl
           and then Get_Global_Decl_Value (Our_Gidx) = V
         loop
            Write_C_Line (Our_Gidx);
            Our_Gidx := Our_Gidx + 1;
         end loop;
      end Write_One_Declaration;

      ----------------------
      -- Maybe_Write_Decl --
      ----------------------

      procedure Maybe_Write_Decl (V : Value_T) is
         Pos : Value_To_Decl_Maps.Cursor;

      begin
         if No (V)
           or else (not Is_A_Global_Variable (V)
                    and then not Is_A_Function (V))
         then
            return;
         end if;

         --  See if V is in the list of deferred decls

         Pos := Find (Declaration_Map, V);
         if Has_Element (Pos) then
            declare
               Gidx : constant Global_Decl_Idx := Element (Pos);

            begin
               Write_One_Declaration (Gidx, V);
            end;
         end if;
      end Maybe_Write_Decl;

   begin -- Start of processing for Write_C_File

      --  Make a pass over the items in source and record for which we
      --  have declarations (objects and subprograms) and for which we
      --  have definitions (subprograms). We may have a duplicate entry in
      --  renaming cases.

      for J in 1 .. Source_Order.Last loop
         declare
            Defining : Boolean;
            V        : constant Value_T := Referenced_Value (J, Defining);

         begin
            if Present (V) then
               if Defining then
                  Include (Definition_Map,  V, Empty_Subprogram_Idx);
               else
                  Include (Declaration_Map, V, Empty_Global_Decl_Idx);
               end if;
            end if;
         end;
      end loop;

      --  We write out typedefs first

      for Tidx in Typedef_Idx_Start .. Get_Last_Typedef loop
         Write_C_Line (Tidx);
      end loop;

      --  If we wrote some typedefs, write a blank line

      if Get_Last_Typedef >= Typedef_Idx_Start then
         Write_Eol;
      end if;

      --  Next write out the global decls other than those that are
      --  present in the source order table. For those, we record the first
      --  index for a definition.

      for Gidx in Global_Decl_Idx_Start .. Get_Last_Global_Decl loop
         declare
            V   : constant Value_T                    :=
              Get_Global_Decl_Value (Gidx);
            Pos : constant Value_To_Decl_Maps.Cursor :=
              (if   Present (V) then Find (Declaration_Map, V)
               else Value_To_Decl_Maps.No_Element);

         begin
            if not Has_Element (Pos) then

               --  First see if this forces us to declare anything that's
               --  pending. Only check variables here because the
               --  declaration of a function doesn't reference objects
               --  inside it.

               if Present (V) and then not Is_A_Function (V) then
                  Scan_For_Decls (V);
               end if;

               Write_C_Line (Gidx);
            elsif No (Element (Pos)) then
               Replace (Declaration_Map, V, Gidx);
            end if;
         end;
      end loop;

      --  Now write out each subprogram, except for those that are present
      --  in the source index table or are an elab proc.

      for Sidx in Subprogram_Idx_Start .. Subprograms.Last loop
         declare
            V   : constant Value_T                    :=
              Subprograms.Table (Sidx).Func;
            Pos : constant Value_To_Subprogram_Maps.Cursor :=
              (if   Present (V) then Find (Definition_Map, V)
               else Value_To_Subprogram_Maps.No_Element);

         begin
            if V = Elab_Spec_Func then
               Elab_Spec_SI := Sidx;
            elsif V = Elab_Body_Func then
               Elab_Body_SI := Sidx;
            elsif not Has_Element (Pos) then
               Write_One_Subprogram (Sidx);
            elsif No (Element (Pos)) then
               Replace (Definition_Map, V, Sidx);
            end if;
         end;
      end loop;

      --  Now write objects from source in source order, interspersing
      --  declarations, definitions, and annotation pragmas.

      for J in 1 .. Source_Order.Last loop
         declare
            Defining : Boolean;
            V        : constant Value_T := Referenced_Value (J, Defining);
            N        : constant Node_Id := Source_Order.Table (J);

         begin
            if Present (V) then
               if Defining then
                  declare
                     Pos : constant Value_To_Subprogram_Maps.Cursor :=
                       Find (Definition_Map, V);

                  begin
                     if Has_Element (Pos) and then Present (Element (Pos)) then
                        Write_One_Subprogram (Element (Pos));
                     end if;
                  end;
               else
                  declare
                     Pos : constant Value_To_Decl_Maps.Cursor :=
                       Find (Declaration_Map, V);

                  begin
                     if Has_Element (Pos) and then Present (Element (Pos)) then
                        Write_One_Declaration (Element (Pos), V);
                     end if;
                  end;
               end if;

            elsif Nkind (N) = N_Pragma then
               declare
                  Ann_Idx : constant Nat := Create_Annotation (N);

               begin
                  if Ann_Idx /= 0 then
                     Output_Annotation
                       (Ann_Idx, No_Value_T, Is_Global => True);
                  end if;
               end;
            end if;
         end;
      end loop;

      --  Finally, write each elab proc, if we have it

      if Present (Elab_Spec_SI) then
         Write_One_Subprogram (Elab_Spec_SI);
      end if;
      if Present (Elab_Body_SI) then
         Write_One_Subprogram (Elab_Body_SI);
      end if;

   end Write_C_File;

begin
   --  Ensure we have an empty entry in the subprogram table

   Subprograms.Increment_Last;
end CCG.Subprograms;
