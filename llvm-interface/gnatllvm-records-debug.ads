------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                  Copyright (C) 2025, AdaCore                             --
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

package GNATLLVM.Records.Debug is
   --  Subpackage for creating the LLVM debuginfo for a given record.

   function Create_Record_Debug_Info (TE : Void_Or_Type_Kind_Id;
                                      Original_Type : Entity_Id;
                                      Debug_Scope : Metadata_T;
                                      Name : String;
                                      Size : ULL;
                                      Align : Nat;
                                      S : Source_Ptr) return Metadata_T;
   --  Create the LLVM debuginfo for a given record.

end GNATLLVM.Records.Debug;
