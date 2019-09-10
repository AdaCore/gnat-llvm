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

with System;

with stdint_h; use stdint_h;

with Interfaces.C;
with Interfaces.C.Extensions;

with Atree;    use Atree;
with Einfo;    use Einfo;
with Namet;    use Namet;
with Types;    use Types;
with Uintp;    use Uintp;

with LLVM.Target;         use LLVM.Target;
with LLVM.Target_Machine; use LLVM.Target_Machine;
with LLVM.Types;          use LLVM.Types;

package GNATLLVM is

   --  Note: in order to find the right LLVM instruction to generate,
   --  you can compare with what Clang generates on corresponding C or C++
   --  code. This can be done online via http://ellcc.org/demo/index.cgi

   --  See also DragonEgg sources for comparison on how GCC nodes are converted
   --  to LLVM nodes: http://llvm.org/svn/llvm-project/dragonegg/trunk

   --  This package contains very low-level types, objects, and functions,
   --  mostly corresponding to LLVM objects and exists to avoid circular
   --  dependencies between other specs.  The intent is that every child
   --  package of LLVM with's this child, but that this with's no other
   --  children.

   --  Define unsigned and unsigned_long_long here instead of use'ing
   --  Interfaces.C and Interfaces.C.Extensions because the latter brings
   --  in a lot of junk that gets in the way and the former conflicts with
   --  Int from the front end.

   subtype unsigned is Interfaces.C.unsigned;
   function "+" (L, R : unsigned)  return unsigned renames Interfaces.C."+";
   function "-" (L, R : unsigned)  return unsigned renames Interfaces.C."-";
   function "*" (L, R : unsigned)  return unsigned renames Interfaces.C."*";
   function "/" (L, R : unsigned)  return unsigned renames Interfaces.C."/";
   function "=" (L, R : unsigned)  return Boolean  renames Interfaces.C."=";
   function ">" (L, R : unsigned)  return Boolean  renames Interfaces.C.">";
   function "<" (L, R : unsigned)  return Boolean  renames Interfaces.C."<";
   function "<=" (L, R : unsigned) return Boolean  renames Interfaces.C."<=";
   function ">=" (L, R : unsigned) return Boolean  renames Interfaces.C.">=";

   subtype unsigned_long_long is Interfaces.C.Extensions.unsigned_long_long;
   subtype ULL is unsigned_long_long;
   --  Define shorter alias name for easier reading

   function "+"  (L, R : ULL)  return ULL renames Interfaces.C.Extensions."+";
   function "-"  (L, R : ULL)  return ULL renames Interfaces.C.Extensions."-";
   function "*"  (L, R : ULL)  return ULL renames Interfaces.C.Extensions."*";
   function "/"  (L, R : ULL)  return ULL renames Interfaces.C.Extensions."/";

   function "**" (L : ULL; R : Integer) return ULL
     renames Interfaces.C.Extensions."**";
   function "mod"  (L, R : ULL)  return ULL
     renames Interfaces.C.Extensions."mod";
   function "and"  (L, R : ULL)  return ULL
     renames Interfaces.C.Extensions."and";

   function "="  (L, R : ULL)  return Boolean
     renames Interfaces.C.Extensions."=";
   function ">"  (L, R : ULL)  return Boolean
     renames Interfaces.C.Extensions.">";
   function "<"  (L, R : ULL)  return Boolean
     renames Interfaces.C.Extensions."<";
   function "<=" (L, R : ULL) return  Boolean
     renames Interfaces.C.Extensions."<=";
   function ">=" (L, R : ULL) return  Boolean
     renames Interfaces.C.Extensions.">=";

   subtype long_long_integer is Interfaces.C.Extensions.long_long;
   subtype LLI is long_long_integer;
   --  Define shorter alias name for easier reading

   function UI_From_LLI is new UI_From_Integral (LLI);
   --  And conversion routine from it to UI

   type MD_Builder_T is new System.Address;
   --  Metadata builder type: opaque for us

   type Value_Array        is array (Nat range <>) of Value_T;
   type Basic_Block_Array  is array (Nat range <>) of Basic_Block_T;
   type Index_Array        is array (Nat range <>) of unsigned;
   type Access_Value_Array is access all Value_Array;

   No_Value_T    : constant Value_T       := Value_T (System.Null_Address);
   No_Type_T     : constant Type_T        := Type_T (System.Null_Address);
   No_BB_T       : constant Basic_Block_T :=
     Basic_Block_T (System.Null_Address);
   No_Metadata_T : constant Metadata_T    := Metadata_T (System.Null_Address);
   No_Builder_T  : constant Builder_T     := Builder_T (System.Null_Address);
   No_Module_T   : constant Module_T      := Module_T (System.Null_Address);
   --  Constant for null objects of various types

   function No (V : Value_T)            return Boolean is (V = No_Value_T);
   function No (T : Type_T)             return Boolean is (T = No_Type_T);
   function No (B : Basic_Block_T)      return Boolean is (B = No_BB_T);
   function No (M : Metadata_T)         return Boolean is (M = No_Metadata_T);
   function No (M : Builder_T)          return Boolean is (M = No_Builder_T);
   function No (N : Name_Id)            return Boolean is (N = No_Name);

   function Present (V : Value_T)       return Boolean is (V /= No_Value_T);
   function Present (T : Type_T)        return Boolean is (T /= No_Type_T);
   function Present (B : Basic_Block_T) return Boolean is (B /= No_BB_T);
   function Present (M : Metadata_T)    return Boolean is (M /= No_Metadata_T);
   function Present (M : Builder_T)     return Boolean is (M /= No_Builder_T);
   function Present (N : Name_Id)       return Boolean is (N /= No_Name);
   --  Test for presence and absence of fields of LLVM types

   function No      (U : Uint) return Boolean is (U = No_Uint);
   function Present (U : Uint) return Boolean is (U /= No_Uint);
   --  Add presence and absense functions for Uint for consistency

   function Is_Type_Or_Void (E : Entity_Id) return Boolean is
     (Ekind (E) = E_Void or else Is_Type (E));
   --  We can have Etype's that are E_Void for E_Procedure

   --  We have to define GL_Type here so it can be used in GNATLLVM.GLValue,
   --  which GNATLLVM.GLType must reference.

   GL_Type_Low_Bound  : constant := 700_000_000;
   GL_Type_High_Bound : constant := 799_999_999;
   type GL_Type is range GL_Type_Low_Bound .. GL_Type_High_Bound;
   No_GL_Type         : constant GL_Type := GL_Type_Low_Bound;

   function No      (GT : GL_Type) return Boolean is (GT =  No_GL_Type);
   function Present (GT : GL_Type) return Boolean is (GT /= No_GL_Type);

   type Word_Array is array (Nat range <>) of aliased uint64_t;
   --  Array of words for LLVM construction functions

   Context            : Context_T;
   --  The current LLVM Context

   IR_Builder         : Builder_T;
   --  The current LLVM Instruction builder

   DI_Builder          : DI_Builder_T;
   --  The current LLVM Debug Info builder

   Module             : Module_T;
   --  The LLVM Module being compiled

   LLVM_Target        : Target_T;
   --  The LLVM target for our module

   Target_Machine     : Target_Machine_T;
   --  The LLVM target machine for our module

   MD_Builder         : MD_Builder_T;
   --  The current LLVM Metadata builder

   TBAA_Root          : Metadata_T;
   --  Root of tree for Type-Based alias Analysis (TBAA) metadata

   Module_Data_Layout : Target_Data_T;
   --  LLVM current module data layout.

   Convert_Module     : Module_T;
   --  The module use by Convert_Nonsymbolic_Constant

   Size_Type          : Entity_Id := Empty;
   Size_GL_Type       : GL_Type   := No_GL_Type;
   LLVM_Size_Type     : Type_T    := No_Type_T;
   --  Types to use for sizes

   Void_Ptr_Type      : Type_T;
   --  Pointer to arbitrary memory (we use i8 *); equivalent of
   --  Standard_A_Char.

   A_Char_GL_Type    : GL_Type;
   Boolean_GL_Type   : GL_Type;
   SSI_GL_Type       : GL_Type;
   SI_GL_Type        : GL_Type;
   Integer_GL_Type   : GL_Type;
   LI_GL_Type        : GL_Type;
   LLI_GL_Type       : GL_Type;
   Void_GL_Type      : GL_Type;
   Any_Array_GL_Type : GL_Type;
   --  GL_Types for builtin types

   Int_32_GL_Type    : GL_Type;
   Int_32_Type       : Entity_Id;
   --  Type for 32-bit integers (for GEP indexes)

   BPU                : Int;
   Bit_T              : Type_T;
   Byte_T             : Type_T;

   --  GNAT LLVM switches

   Decls_Only      : Boolean        := False;
   --  True if just compiling to process and back-annotate decls

   Output_Assembly : Boolean := False;
   --  True if -S was specified

   Emit_LLVM       : Boolean := False;
   --  True if -emit-llvm was specified

   Emit_Debug_Info : Boolean := False;
   --  Whether or not to emit debugging information (-g)

   Do_Stack_Check  : Boolean := False;
   --  If set, check for too-large allocation

   Short_Enums     : Boolean := False;
   --  True if we should use the RM_Size, not Esize, for enums

   subtype Err_Msg_Type is String (1 .. 10000);
   type Ptr_Err_Msg_Type is access all Err_Msg_Type;
   --  Used for LLVM error handling

end GNATLLVM;
