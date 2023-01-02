------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S D E F A U L T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1998-2023, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
------------------------------------------------------------------------------

--  This is the GNAT-to-LLVM version of package body Sdefault.

--  This package body provides the llvm-gnat1 implementation of the routines
--  that locate the Ada library source and object directories.

with LLVM.Target_Machine; use LLVM.Target_Machine;

with Options; use Options;
with Osint; use Osint;

package body Sdefault is
   pragma Style_Checks (Off);

   ------------------------------
   -- Include_Dir_Default_Name --
   ------------------------------

   function Include_Dir_Default_Name return String_Ptr is
   begin
      return Relocate_Path ("/PREFIX",
                            (if CCG then "/PREFIX/lib/rts-ccg/adainclude"
                             else "/PREFIX/lib/rts-native/adainclude"));
   end Include_Dir_Default_Name;

   -----------------------------
   -- Object_Dir_Default_Name --
   -----------------------------

   function Object_Dir_Default_Name return String_Ptr is
   begin
      return Relocate_Path ("/PREFIX",
                            (if CCG then "/PREFIX/lib/rts-ccg/adalib"
                             else "/PREFIX/lib/rts-native/adalib"));
   end Object_Dir_Default_Name;

   -----------------------
   -- Search_Dir_Prefix --
   -----------------------

   function Search_Dir_Prefix return String_Ptr is
   begin
      return Relocate_Path ("/PREFIX", "/PREFIX/lib/");
   end Search_Dir_Prefix;

   -----------------
   -- Target_Name --
   -----------------

   function Target_Name return String_Ptr is
   begin
      return new String'(Get_Default_Target_Triple);
   end Target_Name;

end Sdefault;
