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

with Atree; use Atree;
with Types; use Types;

with LLVM.Types;      use LLVM.Types;

with GNATLLVM.Environment; use GNATLLVM.Environment;

package GNATLLVM.DebugInfo is

   Emit_Debug_Info : Boolean := False;
   --  Whether or not to emit debugging information (-g)

   type DI_File_Cache is array (Source_File_Index range <>) of Metadata_T;
   DI_Cache : access DI_File_Cache := null;

   procedure Push_Debug_Scope (Scope : Metadata_T)
     with Pre => not Emit_Debug_Info or else Present (Scope);
   --  Push the current debug scope and make Scope the present scope.  Does
   --  nothing if not debugging.

   procedure Pop_Debug_Scope;
   --  Pop the debugging scope.  Does nothing if not debugging.

   procedure Initialize_Debugging (Env : Environ)
     with Pre => Env /= null;
   --  Set up the environment for generating debugging information

   procedure Finalize_Debugging (Env : Environ)
     with Pre => Env /= null;
   --  Finalize the debugging info at the end of the translation

   function Get_Debug_File_Node
     (Env : Environ; File : Source_File_Index) return Metadata_T
     with Pre  => Env /= null,
          Post => not Emit_Debug_Info
                  or else Present (Get_Debug_File_Node'Result);
   --  Produce and return a DIFile entry for the specified source file index

   function Create_Subprogram_Debug_Info
     (Env            : Environ;
      Func           : GL_Value;
      Def_Ident      : Entity_Id;
      N              : Node_Id;
      Name, Ext_Name : String) return Metadata_T
     with Pre  => Env /= null and then Present (Func)
                  and then Present (Def_Ident) and then Present (N),
          Post => not Emit_Debug_Info
                  or else Present (Create_Subprogram_Debug_Info'Result);
   --  Create debugging information for Func with entity Def_Ident using
   --  the line number information in N for the location and with the
   --  specified internal and external names.

   procedure Push_Lexical_Debug_Scope (Env : Environ; N : Node_Id)
     with Pre => Env /= null and Present (N);
   --  Push a lexical scope starting at N into the debug stack

   procedure Set_Debug_Pos_At_Node (Env : Environ; N : Node_Id)
     with Pre => Env /= null and then Present (N);
   --  Set builder position for debugging to the Sloc of N.

end GNATLLVM.DebugInfo;
