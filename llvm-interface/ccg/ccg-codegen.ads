------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                     --
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

package CCG.Codegen is

   type Inline_Header is (None, Inline_Always, Inline);
   --  Says whether to output no function to .h file, only those that are
   --  are marked as Inline_Always, or those plus ones marked Inline.

   Target_Info_File  : String_Access  := null;
   --  If non-null, the name of a file from which to read C target parameters

   Dump_C_Parameters : Boolean        := False;
   --  True if we should dump the values of the C target parameters

   C_Parameter_File  : String_Access  := null;
   --  If non-null, the name of a file to dump the C parameters

   Emit_Header       : Boolean        := False;
   --  If True, emit header to .h file

   Header_Inline     : Inline_Header  := None;
   --  Says which inline functions to write to .h file

   Emit_C_Line        : Boolean := False;
   --  When generating C code, indicates that we want to generate #line
   --  directives. This corresponds to -g.

   Inlines_In_Header  : Boolean := False;
   --  If True, we have at least one inline function in the header file

   procedure Initialize_Output;
   --  Do any initialization needed to output C.  This is always called after
   --  we've obtained target parameters.

   procedure Generate (Module : Module_T);
   --  The main procedure, which generates C code from the LLVM IR

   function Process_Switch (Switch : String) return Boolean;
   --  S is a switch passed to GNAT LLVM. If it's a switch meaningful
   --  to us, process it and return True.

end CCG.Codegen;
