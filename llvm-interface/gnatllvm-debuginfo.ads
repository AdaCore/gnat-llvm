------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2019, AdaCore                     --
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

with LLVM.Debug_Info; use LLVM.Debug_Info;

with GNATLLVM.GLValue; use GNATLLVM.GLValue;

package GNATLLVM.DebugInfo is

   Debug_Compile_Unit  : Metadata_T;
   --  DICompilleUnit metadata for the main compile unit

   type DI_File_Cache is array (Source_File_Index range <>) of Metadata_T;
   type DI_File_Cache_Access is access all DI_File_Cache;

   DI_Cache : DI_File_Cache_Access := null;

   procedure Push_Debug_Scope (SFI : Source_File_Index; Scope : Metadata_T)
     with Pre => not Emit_Debug_Info or else Present (Scope);
   --  Push the current debug scope and make Scope the present scope.  Does
   --  nothing if not debugging.

   procedure Pop_Debug_Scope;
   --  Pop the debugging scope.  Does nothing if not debugging.

   procedure Initialize;
   --  Set up the environment for generating debugging information

   procedure Finalize_Debugging;
   --  Finalize the debugging info at the end of the translation

   function Get_Debug_File_Node (File : Source_File_Index) return Metadata_T
     with Post => not Emit_Debug_Info
                  or else Present (Get_Debug_File_Node'Result);
   --  Produce and return a DIFile entry for the specified source file index

   function Create_Subprogram_Debug_Info
     (Func           : GL_Value;
      Def_Ident      : Entity_Id;
      N              : Node_Id;
      Name, Ext_Name : String) return Metadata_T
     with Pre  => Present (Func) and then Present (Def_Ident)
                  and then Present (N),
          Post => not Emit_Debug_Info
                  or else Present (Create_Subprogram_Debug_Info'Result);
   --  Create debugging information for Func with entity Def_Ident using
   --  the line number information in N for the location and with the
   --  specified internal and external names.

   procedure Push_Lexical_Debug_Scope (N : Node_Id)
     with Pre => Present (N);
   --  Push a lexical scope starting at N into the debug stack

   procedure Set_Debug_Pos_At_Node (N : Node_Id)
     with Pre => Present (N);
   --  Set builder position for debugging to the Sloc of N.

   procedure Push_Debug_Freeze_Pos;
   procedure Pop_Debug_Freeze_Pos;
   --  When we're doing expansion for computing sizes and/or field positions,
   --  we'll sometimes be going into nodes whose Sloc is at the point of
   --  definition of a type.  Jumping to that Sloc is not helpful so these
   --  calls should be used to freeze the position.  Each "push" must be
   --  cancelled with a "pop" and the position will be frozen until the
   --  all pushes have been popped.

   function Create_Debug_Type_Data (GT : GL_Type) return Metadata_T
     with Pre => Present (GT);
   --  Create metadata corresponding to the type of GT.  Return
   --  No_Metadata_T if the type is too complex.

   procedure Build_Global_Variable_Debug_Data
     (Def_Ident : Entity_Id; V : GL_Value)
     with Pre => not Is_Type (Def_Ident) and then Present (V);
   --  Build debugging data for Def_Ident, a global variable, with V as its
   --  location.

private

   subtype UL is Interfaces.C.unsigned_long;

   function "+" (L, R : UL)  return UL renames Interfaces.C."+";
   function "-" (L, R : UL)  return UL renames Interfaces.C."-";
   function "*" (L, R : UL)  return UL renames Interfaces.C."*";
   function "/" (L, R : UL)  return UL renames Interfaces.C."/";
   function "=" (L, R : UL)  return Boolean  renames Interfaces.C."=";
   function ">" (L, R : UL)  return Boolean  renames Interfaces.C.">";
   function "<" (L, R : UL)  return Boolean  renames Interfaces.C."<";
   function "<=" (L, R : UL) return Boolean  renames Interfaces.C."<=";
   function ">=" (L, R : UL) return Boolean  renames Interfaces.C.">=";

   --  Define the various Dwarf type attributes.  This is encoded in
   --  lvm/BinaryFormat/Dwarf.def, but it's simpler to just repeat them
   --  here since they are part of the standard and won't change.

   DW_ATE_Address         : constant DWARF_Type_Encoding_T := 16#01#;
   DW_ATE_Boolean         : constant DWARF_Type_Encoding_T := 16#02#;
   DW_ATE_Complex_Float   : constant DWARF_Type_Encoding_T := 16#03#;
   DW_ATE_Float           : constant DWARF_Type_Encoding_T := 16#04#;
   DW_ATE_Signed          : constant DWARF_Type_Encoding_T := 16#05#;
   DW_ATE_Signed_Char     : constant DWARF_Type_Encoding_T := 16#06#;
   DW_ATE_Unsigned        : constant DWARF_Type_Encoding_T := 16#07#;
   DW_ATE_Unsigned_Char   : constant DWARF_Type_Encoding_T := 16#08#;
   DW_ATE_Imaginary_Float : constant DWARF_Type_Encoding_T := 16#09#;
   DW_ATE_Packed_Decimal  : constant DWARF_Type_Encoding_T := 16#0A#;
   DW_ATE_Numeric_String  : constant DWARF_Type_Encoding_T := 16#0B#;
   DW_ATE_Edited          : constant DWARF_Type_Encoding_T := 16#0C#;
   DW_ATE_Signed_Fixed    : constant DWARF_Type_Encoding_T := 16#0D#;
   DW_ATE_Unsigned_Fixed  : constant DWARF_Type_Encoding_T := 16#0E#;
   DW_ATE_Decimal_Float   : constant DWARF_Type_Encoding_T := 16#0F#;
   DW_ATE_UTF             : constant DWARF_Type_Encoding_T := 16#10#;
   DW_ATE_UCS             : constant DWARF_Type_Encoding_T := 16#11#;
   DW_ATE_ASCII           : constant DWARF_Type_Encoding_T := 16#12#;

end GNATLLVM.DebugInfo;
