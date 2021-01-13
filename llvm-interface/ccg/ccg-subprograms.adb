------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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
with Interfaces.C; use Interfaces.C;

with Output; use Output;
with Table;

with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

with CCG.Aggregates;   use CCG.Aggregates;
with CCG.Blocks;       use CCG.Blocks;
with CCG.Instructions; use CCG.Instructions;
with CCG.Output;       use CCG.Output;
with CCG.Tables;       use CCG.Tables;
with CCG.Utils;        use CCG.Utils;

package body CCG.Subprograms is

   type Decl_Idx is new Nat;
   type Stmt_Idx is new Nat;

   No_Decl_Idx : constant Decl_Idx := 0;
   No_Stmt_Idx : constant Stmt_Idx := 0;

   function Present (J : Decl_Idx) return Boolean is (J /= No_Decl_Idx);
   function Present (J : Stmt_Idx) return Boolean is (J /= No_Stmt_Idx);
   function No      (J : Decl_Idx) return Boolean is (J = No_Decl_Idx);
   function No      (J : Stmt_Idx) return Boolean is (J = No_Stmt_Idx);

   --  Tables for decls and statements

   package Decl_Table is new Table.Table
     (Table_Component_Type => Str,
      Table_Index_Type     => Decl_Idx,
      Table_Low_Bound      => 1,
      Table_Initial        => 500,
      Table_Increment      => 100,
      Table_Name           => "Decl_Table");

   package Stmt_Table is new Table.Table
     (Table_Component_Type => Str,
      Table_Index_Type     => Stmt_Idx,
      Table_Low_Bound      => 1,
      Table_Initial        => 1000,
      Table_Increment      => 1000,
      Table_Name           => "Stmt_Table");

   --  For each subprogram, we record the first and last decl and statement
   --  belonging to that subprogram.

   type Subprogram_Data is record
      Func       : Value_T;
      First_Decl : Decl_Idx;
      Last_Decl  : Decl_Idx;
      First_Stmt : Stmt_Idx;
      Last_Stmt  : Stmt_Idx;
   end record;

   type Subprogram_Idx is new Nat;

   package Subprogram_Table is new Table.Table
     (Table_Component_Type => Subprogram_Data,
      Table_Index_Type     => Subprogram_Idx,
      Table_Low_Bound      => 1,
      Table_Initial        => 50,
      Table_Increment      => 50,
      Table_Name           => "Subprogram_Table");

   function Is_Builtin_Name (S : String) return Boolean is
     (S'Length > 5 and then S (S'First .. S'First + 4) = "llvm.");

   function Call_Builtin
     (V : Value_T; S : String; Ops : Value_Array) return Boolean
      with Pre => Get_Opcode (V) = Op_Call;
   --  Call V, a call to a builtin function whose name is S, with operands
   --  of Ops.
   --  Return True if the builtin is handled and supported, False otherwise.

   function Effective_Return_Type (T : Type_T) return Str
     with Pre  => Get_Type_Kind (T) = Function_Type_Kind,
          Post => Present (Effective_Return_Type'Result);
   --  Return a string corresponding to the return type of T, adjusting the
   --  type in the case where it's an array.

   -----------------
   -- Output_Decl --
   ----------------

   procedure Output_Decl (S : Str; Semicolon : Boolean := True) is
      SD : Subprogram_Data renames
        Subprogram_Table.Table (Subprogram_Table.Last);

   begin
      Decl_Table.Append ((if Semicolon then S & ";" else S));
      SD.Last_Decl := Decl_Table.Last;
      if No (SD.First_Decl) then
         SD.First_Decl := Decl_Table.Last;
      end if;
   end Output_Decl;

   -----------------
   -- Output_Decl --
   ----------------

   procedure Output_Decl (S : String; Semicolon : Boolean := True) is
   begin
      Output_Decl (+S, Semicolon);
   end Output_Decl;

   -----------------
   -- Output_Stmt --
   ----------------

   procedure Output_Stmt (S : Str; Semicolon : Boolean := True) is
      SD : Subprogram_Data renames
        Subprogram_Table.Table (Subprogram_Table.Last);
   begin
      Stmt_Table.Append ((if Semicolon then S & ";" else S));
      SD.Last_Stmt := Stmt_Table.Last;
      if No (SD.First_Stmt) then
         SD.First_Stmt := Stmt_Table.Last;
      end if;
   end Output_Stmt;

   -----------------
   -- Output_Stmt --
   ----------------

   procedure Output_Stmt (S : String; Semicolon : Boolean := True) is
   begin
      Output_Stmt (+S, Semicolon);
   end Output_Stmt;

   --------------------
   -- New_Subprogram --
   --------------------

   procedure New_Subprogram (V : Value_T) is
   begin
      Subprogram_Table.Append ((Func       => V,
                                First_Decl => No_Decl_Idx,
                                Last_Decl  => No_Decl_Idx,
                                First_Stmt => No_Stmt_Idx,
                                Last_Stmt  => No_Stmt_Idx));
   end New_Subprogram;

   ------------------
   -- Current_Func --
   ------------------

   function Current_Func return Value_T is
     (Subprogram_Table.Table (Subprogram_Table.Last).Func);

   ---------------------------------
   -- Write_Function_Type_Typedef --
   ---------------------------------

   procedure Write_Function_Type_Typedef (T : Type_T) is
      Fn_T : constant Type_T := Get_Element_Type (T);

   begin
      Write_Str ("typedef " & Function_Proto (Fn_T, "(*" & T & ")") & ";",
                 Eol => True);
   end Write_Function_Type_Typedef;

   ---------------------------
   -- Effective_Return_Type --
   ---------------------------

   function Effective_Return_Type (T : Type_T) return Str is
      Ret_Typ : constant Type_T := Get_Return_Type (T);

   begin
      --  If this function returns an array, change it to return a
      --  struct containing that array.

      if Get_Type_Kind (Ret_Typ) = Array_Type_Kind then
         Maybe_Write_Array_Return_Typedef (Ret_Typ);
         return Ret_Typ & "_R";
      else
         return +Ret_Typ;
      end if;
   end Effective_Return_Type;

   --------------------
   -- Function_Proto --
   --------------------

   function Function_Proto
     (V : Value_T; Extern : Boolean := False) return Str
   is
      Num_Params : constant Nat    := Count_Params (V);
      Fn_Typ     : constant Type_T := Get_Element_Type (V);
      Result     : Str             :=
        Effective_Return_Type (Fn_Typ) & " " & V & " (";

   begin
      --  If this is an internal subprogram, mark it as static

      if Get_Linkage (V) = Internal_Linkage then
         Result := "static " & Result;
      end if;

      --  If this doesn't return mark that

      if Does_Not_Return (V) then
         Result := "__attribute__((noreturn)) " & Result;
      end if;

      --  Indicate if this returns unsigned

      if Get_Is_Unsigned (V) then
         Result := "unsigned " & Result;
      end if;

      --  Then output the list of parameter types, if any.  If this isn't
      --  for an extern definition, include the parameter names.

      if Num_Params = 0 then
         Result := Result & "void";
      else
         for J in 0 .. Num_Params - 1 loop
            declare
               Param : constant Value_T := Get_Param (V, J);

            begin
               Result :=
                 Result & (if J = 0 then "" else ", ") & (Param + Write_Type);

               if not Extern then
                  Result := Result & " " & Param;
               end if;
            end;
         end loop;
      end if;

      return Result & ")";
   end Function_Proto;

   --------------------
   -- Function_Proto --
   --------------------

   function Function_Proto (T : Type_T; S : Str) return Str is
      Num_Params : constant Nat    := Count_Param_Types (T);
      P_Types    : Type_Array (1 .. Num_Params);
      First      : Boolean         := True;
      Result     : Str             :=
        Effective_Return_Type (T) & " " & S & " (";

   begin
      if Num_Params = 0 then
         Result := Result & "void";
      else
         Get_Param_Types (T, P_Types'Address);
         for T of P_Types loop
            begin
               Result := Result & (if First then "" else ", ") & T;
               First := False;
            end;
         end loop;
      end if;

      return Result & ")";

   end Function_Proto;

   ------------------------
   -- Declare_Subprogram --
   ------------------------

   procedure Declare_Subprogram (V : Value_T) is
      S        : constant String        := Get_Value_Name (V);

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
      --  basic blocks, it must be an extern.

      Write_Str ((if No (Get_First_Basic_Block (V)) then "extern " else "") &
        Function_Proto (V, Extern => True) & ";" & Eol_Str);

   end Declare_Subprogram;

   -------------------------------
   -- Generate_C_For_Subprogram --
   -------------------------------

   procedure Generate_C_For_Subprogram (V : Value_T) is
   begin
      --  If there is an entry basic block, start a new function and
      --  output it, starting from that block.

      if Present (Get_First_Basic_Block (V)) then
         New_Subprogram (V);
         Output_BB (Get_Entry_Basic_Block (V));
      end if;

      --  There shouldn't be anything still pending now, but if there is,
      --  output it now since if we hold it to the next subprogram, it'll
      --  reference variables in this one.

      Process_Pending_Values;
   end Generate_C_For_Subprogram;

   ------------------
   -- Call_Builtin --
   ------------------

   type Arithmetic_Operation is (Add, Subtract);
   --  For now only consider Add/Sub with overflow

   Overflow_Declared : array (Arithmetic_Operation) of Boolean :=
     (others => False);

   function Call_Builtin
     (V : Value_T; S : String; Ops : Value_Array) return Boolean
   is
      Op1    : constant Value_T  :=
        (if Ops'Length >= 1 then Ops (Ops'First) else No_Value_T);
      Op2    : constant Value_T  :=
        (if Ops'Length >= 2 then Ops (Ops'First + 1) else No_Value_T);
      Op3    : constant Value_T  :=
        (if Ops'Length >= 3 then Ops (Ops'First + 2) else No_Value_T);
      Result : Str;

      procedure Op_With_Overflow (Arit : Arithmetic_Operation);
      --  Handle an arithmetic operation with overflow

      ----------------------
      -- Op_With_Overflow --
      ----------------------

      procedure Op_With_Overflow (Arit : Arithmetic_Operation) is
         Subp : constant String :=
           "system__arith_64__" & To_Lower (Arit'Image) &
           "_with_ovflo_check64";
         Bits : constant unsigned :=
           unsigned'Value (S (S'First + 25 .. S'Last));

      begin
         Maybe_Decl (V);

         if not Overflow_Declared (Arit) then
            Write_Str
              ("extern long long " & Subp & " (long long, long long);");
            Write_Eol;
            Overflow_Declared (Arit) := True;
         end if;

         --  Overflow builtins are only generated by the LLVM optimizer (see
         --  lib/Transforms/InstCombineCompares.cpp), but we still want to
         --  handle them by calling the routines in System.Arith_64 even if
         --  this is clearly inefficient.
         --  The LLVM builtin deals with a struct containing two fields: the
         --  first is the integer result, the second is the overflow bit.
         --  If the Arith_64 routine succeeds (does not raise an exception),
         --  it means that no overflow occurred so always clear the second
         --  field.
         --  ??? The front end is able to convert some overflow operations
         --  to direct comparison. We ought to do the same here. And if
         --  we do that, then we can emit the builtins in all cases and
         --  allow the LLVM optimizer to see the builtins, which should allow
         --  it to do a better job. But this is for later work; we need to
         --  get a better idea of the tradeoffs here.

         Write_Copy
           (+V & ".ccg_field_0",
            "(" & Int_String (Pos (Bits)) & ") " & Subp &
            " ((long long) " & Op1 & ", (long long) " & Op2 & ")",
            Int_Type (Bits));
         Write_Copy (+V & ".ccg_field_1", +"0", Int_Type (1));
      end Op_With_Overflow;

   begin
      --  We ignore lifetime start/end calls

      if S (S'First + 5 .. S'First + 12) = "lifetime" then
         return True;

      --  Also ignore stackrestore/stacksave calls: these are generated by the
      --  optimizer and in many cases the stack usage is actually zero or very
      --  low. ??? Not clear that we can do better for now.

      elsif S'Length = 17
        and then S (S'First + 5 .. S'First + 16) = "stackrestore"
      then
         return True;

      elsif S'Length = 14
        and then S (S'First + 5 .. S'First + 13) = "stacksave"
      then
         return True;

      --  Handle some overflow intrinsics

      elsif S'Length > 22
        and then S (S'First + 5 .. S'First + 22) = "sadd.with.overflow"
      then
         Op_With_Overflow (Add);

      elsif S'Length > 22
        and then S (S'First + 5 .. S'First + 22) = "ssub.with.overflow"
      then
         Op_With_Overflow (Subtract);

      --  We process memcpy, memmove, and memset by calling the corresponding
      --  C library function.

      elsif S (S'First + 5 .. S'First + 10) = "memcpy" then
         Result := "memcpy (" & Op1 & ", " & Op2 & ", " & Op3 & ")";
         Process_Pending_Values;
         Output_Stmt (Result);
      elsif S (S'First + 5 .. S'First + 11) = "memmove" then
         Result := "memmove (" & Op1 & ", " & Op2 & ", " & Op3 & ")";
         Process_Pending_Values;
         Output_Stmt (Result);
      elsif S (S'First + 5 .. S'First + 10) = "memset" then
         Result := "memset (" & Op1 & ", " & Op2 & ", " & Op3 & ")";
         Process_Pending_Values;
         Output_Stmt (Result);

      --  And we don't process the rest

      else
         return False;
      end if;

      return True;
   end Call_Builtin;

   ----------------------
   -- Call_Instruction --
   ----------------------

   procedure Call_Instruction (V : Value_T; Ops : Value_Array) is
      Func  : constant Value_T := Ops (Ops'Last);
      S     : constant String  := Get_Value_Name (Func);
      Call  : Str              := Func + Component & " (";
      First : Boolean          := True;

   begin
      --  If this is a builtin, handle that

      if Is_Builtin_Name (S) then
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

            Call :=
              "/* unsupported builtin */ " & (Builtin + Component) & " (";
         end;
      end if;

      --  Otherwise, generate the argument list for the call

      for Op of Ops (Ops'First .. Ops'Last - 1) loop
         if not First then
            Call := Call & ", ";
         end if;

         --  If Op is a constant array, we have to cast it to the non-constant
         --  type which is a pointer to the element type.

         if Get_Type_Kind (Op) = Array_Type_Kind and then Get_Is_Constant (Op)
         then
            Call :=
              Call & "(" & Get_Element_Type (Op) & " *) " & (Op + Comma);
         else
            Call := Call & (Op + Comma);
         end if;

         First := False;
      end loop;

      --  Add the final close paren, then write any pending values (we
      --  do it here to avoid writing out things only used as part of
      --  the parameter calculation. If this is a procedure call,
      --  output it. Otherwise, set this as the value of V.

      Call := (Call & ")") + Component;
      Process_Pending_Values;
      if Get_Type_Kind (V) = Void_Type_Kind then
         Output_Stmt (Call);
      else
         --  If this returns an array, we've changed it to returning a
         --  struct whose field is that array, so we need to do the
         --  dereference.  However, we can't simply return the function
         --  call followed by the dereference because if we're going to be
         --  using the entire array, we'll do a memmove and we can't take
         --  the address of the function result.  So we have to make a
         --  variable, assign the function result to it, and dereference
         --  that.  We don't have a value to use as the variable, so we
         --  have to use a bit of a kludge. Note that we can't call
         --  Write_Copy here since it'll think that this is an array copy
         --  and use memmove.

         if Get_Type_Kind (V) = Array_Type_Kind then
            declare
               Our_Var : constant Str := "ccg_v" & Get_Output_Idx;

            begin
               Output_Decl (TP ("#T1_R ", V) & Our_Var);
               Output_Stmt (Our_Var & " = " & Call + Assign);
               Call := Our_Var & ".F" + Component;
            end;
         end if;

         Assignment (V, Call);
      end if;
   end Call_Instruction;

   ------------------------
   -- Return_Instruction --
   ------------------------

   procedure Return_Instruction (V : Value_T; Op : Value_T) is
   begin
      --  If our function is marked no-return, we don't do anything.
      --  Otherwise, handle the cases with and without a value to return.

      if Does_Not_Return (Current_Func) then
         null;
      elsif Present (Op) then

         --  If we're returning an array, declare a value (we can use V even
         --  though its LLVM type is void) of the struct type corresponding
         --  to Op's type, assign Op into its only field (which will be done
         --  with a memmove), and return that value.

         if Get_Type_Kind (Op) = Array_Type_Kind then
            Output_Decl (TP ("#T1_R #2", Op, V));
            Write_Copy (V & ".F", Op, Type_Of (Op));
            Output_Stmt (TP ("return #1", V));
         else
            Output_Stmt ("return " & Op + Assign);
         end if;
      else
         Output_Stmt ("return");
      end if;
   end Return_Instruction;

   -----------------------
   -- Write_Subprograms --
   -----------------------

   procedure Write_Subprograms is
   begin
      for Sidx in 1 .. Subprogram_Table.Last loop
         declare
            SD : constant Subprogram_Data := Subprogram_Table.Table (Sidx);

         begin
            --  First write the prototype

            Write_Str (Function_Proto (SD.Func), Eol => True);
            Write_Str ("{");
            Write_Eol;

            --  Next write the decls, if any

            if Present (SD.First_Decl) then
               for Didx in SD.First_Decl .. SD.Last_Decl loop
                  Write_Str ("    " & Decl_Table.Table (Didx), Eol => True);
               end loop;

               Write_Eol;
            end if;

            --  Then write the statements, if any

            if Present (SD.First_Stmt) then
               for Sidx in SD.First_Stmt .. SD.Last_Stmt loop
                  Write_Str ("    " & Stmt_Table.Table (Sidx), Eol => True);
               end loop;
            end if;

            Write_Str ("}");
            Write_Eol;
         end;
      end loop;
   end Write_Subprograms;

end CCG.Subprograms;
