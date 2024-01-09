------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2023, AdaCore                     --
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

with GNATLLVM.GLValue; use GNATLLVM.GLValue;

package GNATLLVM.Variables is

   type Interface_Names_Id  is new Nat;
   type Global_Dup_Id       is new Nat;
   type Global_Dup_Value_Id is new Nat;

   Empty_Interfaces_Name_Id  : constant Interface_Names_Id  := 0;
   Empty_Global_Dup_Id       : constant Global_Dup_Id       := 0;
   Empty_Global_Dup_Value_Id : constant Global_Dup_Value_Id := 0;

   function Present (Idx : Interface_Names_Id)  return Boolean is (Idx /= 0);
   function Present (Idx : Global_Dup_Id)       return Boolean is (Idx /= 0);
   function Present (Idx : Global_Dup_Value_Id) return Boolean is (Idx /= 0);

   function No (Idx : Interface_Names_Id)       return Boolean is (Idx = 0);
   function No (Idx : Global_Dup_Id)            return Boolean is (Idx = 0);
   function No (Idx : Global_Dup_Value_Id)      return Boolean is (Idx = 0);

   Detected_Duplicates : Boolean := False;

   procedure Register_Global_Name (S : String)
     with Pre => not Detected_Duplicates;
   --  Register that we may be generating a global (variable or subprogram)
   --  of name S. Must be called after we've looked for globals with
   --  Interface_Names. Must not be called twice with the same name.

   procedure Detect_Duplicate_Global_Names;
   --  Make a pass over all library units looking for the use of the same
   --  global name in two different entities and keep a record of all such
   --  duplications.

   function Get_Dup_Global_Value (E : Global_Name_Kind_Id) return GL_Value;
   --  If E corresponds to a duplicated interface name and we've aready
   --  created a global for it, return that global.

   procedure Set_Dup_Global_Value (E : Global_Name_Kind_Id; V : GL_Value)
     with Pre => Present (V);
   --  If E corresponds to a duplicated interface name, record that we've
   --  created a value for it.

   function Get_Dup_Global_Value (S : String) return GL_Value;
   procedure Set_Dup_Global_Value (S : String; V : GL_Value)
     with Pre => Present (V);
   --  Similar, but for strings (for builtins)

   procedure Emit_Decl_Lists
     (List1    : List_Id := No_List;
      List2    : List_Id := No_List;
      End_List : Node_Id := Empty;
      Pass1    : Boolean := True;
      Pass2    : Boolean := True);
   --  Elaborate decls in the lists List1 and List2, if present. We make
   --  two passes, one to elaborate anything other than bodies (but we
   --  declare a function if there was no spec). The second pass
   --  elaborates the bodies.
   --
   --  End_List gives the element in the list past the end. Normally, this
   --  is Empty, but can be First_Real_Statement for a
   --  Handled_Sequence_Of_Statements.
   --
   --  We make a complete pass through both lists if Pass1 is true, then
   --  make the second pass over both lists if Pass2 is true. The lists
   --  usually correspond to the public and private parts of a package.

   function Maybe_Promote_Alloca
     (T : Type_T; Elts : GL_Value := No_GL_Value) return Basic_Block_T
     with Pre => Present (T);
   --  Called when about to do an alloca of type T to see if that
   --  alloca should be promoted to the entry block. The return from
   --  this function must be passed to Done_Promoting_Alloca along
   --  with the alloca immediately after emitting the alloca. The
   --  pair of calls will do what's necessary, either promoting the
   --  alloca or forcing a stack save/restore. If Elts isn't specified,
   --  it's presumed to be 1.
   procedure Done_Promoting_Alloca
     (Alloca : GL_Value;
      BB     : Basic_Block_T;
      T      : Type_T;
      Elts   : GL_Value := No_GL_Value)
     with Pre => Present (Alloca) and then Present (T);

   function Is_Static_Address
     (N : N_Subexpr_Id; Not_Symbolic : Boolean := False) return Boolean;
   --  Return True if N represents an address that can computed statically.
   --  If Not_Symbolic is True, only return if this address is a constant
   --  integer (rare).

   function Static_Address (N : N_Subexpr_Id) return GL_Value;
   --  Return the LLVM representation of the address represented by N, or
   --  report an error and return undefined if N isn't (a conversion of) a
   --  compile-time known address.

   function Is_No_Elab_Needed
     (N              : N_Subexpr_Id;
      Not_Symbolic   : Boolean := False;
      Restrict_Types : Boolean := False) return Boolean
     with Pre => Present (N);
   --  Return True if N represents an expression that can be computed
   --  without needing an elab proc. If Not_Symbolic is True, we also
   --  can't alllow anything symbolic. If Restrict_Types is True, we can't
   --  allow anything that's an access type or an elementary type wider
   --  than a word.

   function Is_No_Elab_Needed_For_Entity
     (E              : Evaluable_Kind_Id;
      Not_Symbolic   : Boolean := False;
      Restrict_Types : Boolean := False) return Boolean;
   --  Return True if E represents an entity that can be computed
   --  without needing an elab proc. If Not_Symbolic is True, we also
   --  can't alllow anything symbolic. If Restrict_Types is True, we can't
   --  allow anything that's an access type or an elementary type wider
   --  than a word.

   function Make_Global_Constant (V : GL_Value) return GL_Value
     with Pre  => not Is_Reference (V),
          Post => Is_A_Global_Variable (Make_Global_Constant'Result)
                  or else Is_Undef (Make_Global_Constant'Result);
   --  Create a global constant that contains the value of V

   procedure Emit_Declaration
     (N : N_Declaration_Id; For_Freeze_Entity : Boolean := False);
   --  Emit a declaration. For_Freeze_Entity is True if we're processing
   --  a Freeze_Entity.

   procedure Emit_Renaming_Declaration (N : N_Renaming_Declaration_Id);
   --  Emit an object or exception renaming declaration

   function Emit_Entity
     (E          : Evaluable_Kind_Id;
      N          : Opt_N_Has_Entity_Id := Empty;
      Prefer_LHS : Boolean             := False) return GL_Value;
   --  Evaluate an entity E. If Present, N is the corresponding N_Identifier
   --  node.  Prefer_LHS is True if we'd prefer this for a LHS context.

end GNATLLVM.Variables;
