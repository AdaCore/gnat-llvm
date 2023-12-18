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

with Einfo.Utils; use Einfo.Utils;
with Repinfo;     use Repinfo;

with GNATLLVM.GLValue; use GNATLLVM.GLValue;

package GNATLLVM.Environment is

   --  Define bounds and types for record, field, and array information

   Record_Info_Low_Bound  : constant := 300_000_000;
   Record_Info_High_Bound : constant := 399_999_999;
   type Record_Info_Id is
     range Record_Info_Low_Bound .. Record_Info_High_Bound;
   Empty_Record_Info_Id   : constant Record_Info_Id := Record_Info_Low_Bound;

   type Record_Info_Id_Array is array (Nat range <>) of Record_Info_Id;
   type Record_Info_Id_Array_Access is access all Record_Info_Id_Array;

   Field_Info_Low_Bound  : constant := 400_000_000;
   Field_Info_High_Bound : constant := 499_999_999;
   type Field_Info_Id is range Field_Info_Low_Bound .. Field_Info_High_Bound;
   Empty_Field_Info_Id   : constant Field_Info_Id := Field_Info_Low_Bound;

   Array_Info_Low_Bound  : constant := 500_000_000;
   Array_Info_High_Bound : constant := 599_999_999;
   type Array_Info_Id is range Array_Info_Low_Bound .. Array_Info_High_Bound;
   Empty_Array_Info_Id   : constant Array_Info_Id := Array_Info_Low_Bound;

   Label_Info_Low_Bound  : constant := 600_000_000;
   Label_Info_High_Bound : constant := 699_999_999;
   type Label_Info_Id is range Label_Info_Low_Bound .. Label_Info_High_Bound;
   Empty_Label_Info_Id   : constant Label_Info_Id := Label_Info_Low_Bound;

   --  700_000_000 .. 7999_999_999 is GL_Type, defined in our parent

   TBAA_Info_Low_Bound   : constant := 800_000_000;
   TBAA_Info_High_Bound  : constant := 899_999_999;
   type TBAA_Info_Id is range TBAA_Info_Low_Bound .. TBAA_Info_High_Bound;
   Empty_TBAA_Info_Id   : constant TBAA_Info_Id := TBAA_Info_Low_Bound;

   function "+" (A : Array_Info_Id; N : Nat) return Array_Info_Id is
     (Array_Info_Id (Nat (A) + N));

   function No (R : Record_Info_Id)      return Boolean is
      (R = Empty_Record_Info_Id);
   function No (F : Field_Info_Id)       return Boolean is
      (F = Empty_Field_Info_Id);
   function No (A : Array_Info_Id)       return Boolean is
      (A = Empty_Array_Info_Id);
   function No (L : Label_Info_Id)       return Boolean is
      (L = Empty_Label_Info_Id);
   function No (T : TBAA_Info_Id)       return Boolean is
      (T = Empty_TBAA_Info_Id);
   function Present (R : Record_Info_Id) return Boolean is
      (R /= Empty_Record_Info_Id);
   function Present (F : Field_Info_Id)  return Boolean is
      (F /= Empty_Field_Info_Id);
   function Present (A : Array_Info_Id)  return Boolean is
      (A /= Empty_Array_Info_Id);
   function Present (L : Label_Info_Id)  return Boolean is
      (L /= Empty_Label_Info_Id);
   function Present (T : TBAA_Info_Id)  return Boolean is
      (T /= Empty_TBAA_Info_Id);

   procedure Initialize_Environment;
   --  Do any needed initializing of the environment

   function Get_GL_Type               (TE : Entity_Id)    return GL_Type
     with Pre => Is_Type_Or_Void (TE), Inline;

   function Get_Associated_GL_Type    (TE : Entity_Id)    return GL_Type
     with Pre => Is_Array_Type (TE) or else Is_Access_Type (TE), Inline;

   function Get_Associated_GL_Type_N  (TE : Entity_Id)    return GL_Type
     with Pre => Is_Array_Type (TE) or else Is_Access_Type (TE), Inline;

   function Is_Nonnative_Type         (TE : Entity_Id)    return Boolean
     with Pre => Is_Type_Or_Void (TE), Inline;

   function Is_Nonnative_Type_N       (TE : Entity_Id)    return Boolean
     with Pre => Is_Type_Or_Void (TE), Inline;

   function Is_Being_Elaborated       (TE : Entity_Id)    return Boolean
     with Pre => Is_Type_Or_Void (TE), Inline;

   function Get_TBAA                  (TE : Entity_Id)    return Metadata_T
     with Pre => Is_Type_Or_Void (TE), Inline;

   function Get_TBAA_N                (TE : Entity_Id)    return Metadata_T
     with Pre => Is_Type_Or_Void (TE), Inline;

   function Get_TBAA_Info             (TE : Entity_Id)    return TBAA_Info_Id
     with Pre => Is_Type_Or_Void (TE), Inline;

   function Get_TBAA_Info_N           (TE : Entity_Id)    return TBAA_Info_Id
     with Pre => Is_Type_Or_Void (TE), Inline;

   function Get_Value                 (VE : Entity_Id)    return GL_Value
     with Pre => Present (VE), Inline;

   function Get_SO_Ref                (N  : N_Subexpr_Id) return Dynamic_SO_Ref
     with Inline;

   function Get_Debug_Type            (TE : Entity_Id)    return Metadata_T
     with Pre => Is_Type_Or_Void (TE), Inline;

   function Get_Debug_Type_N          (TE : Entity_Id)    return Metadata_T
     with Pre => Is_Type_Or_Void (TE), Inline;

   function Get_Array_Info            (TE : Entity_Id)    return Array_Info_Id
     with Pre => Is_Array_Type (TE), Inline;

   function Get_Array_Info_N          (TE : Entity_Id)    return Array_Info_Id
     with Pre => Is_Array_Type (TE), Inline;

   function Get_Orig_Array_Info       (TE : Entity_Id)    return Array_Info_Id
     with Pre => Is_Packed_Array_Impl_Type (TE), Inline;

   function Get_Orig_Array_Info_N     (TE : Entity_Id)    return Array_Info_Id
     with Pre => Is_Packed_Array_Impl_Type (TE), Inline;

   function Get_Record_Info           (TE : Entity_Id)    return Record_Info_Id
     with Pre => Is_Record_Type (TE), Inline;

   function Get_Record_Info_N         (TE : Entity_Id)    return Record_Info_Id
     with Pre => Is_Record_Type (TE), Inline;

   function Get_Field_Info            (VE : Entity_Id)    return Field_Info_Id
     with Pre => Ekind (VE) in Record_Field_Kind, Inline;

   function Get_Label_Info            (VE : Entity_Id)    return Label_Info_Id
     with Pre => Present (VE), Inline;

   function Get_Subprogram_Type       (VE : Entity_Id)    return Type_T
     with Pre => Present (VE), Inline;

   function Get_Flag1                 (VE : Entity_Id)    return Boolean
      with Pre => Present (VE), Inline;

   procedure Set_GL_Type              (TE : Entity_Id; GT : GL_Type)
     with Pre => Is_Type_Or_Void (TE), Post => Get_GL_Type (TE) = GT, Inline;

   procedure Set_Associated_GL_Type   (TE : Entity_Id; GT : GL_Type)
     with Pre  => (Is_Array_Type (TE) or else Is_Access_Type (TE))
                  and then (Present (GT)
                              or else Present (Get_Associated_GL_Type_N (TE)))
                  and then (No (Get_Associated_GL_Type_N (TE))
                              or else Get_Associated_GL_Type_N (TE) = GT
                              or else No (GT)),
          Post => Get_Associated_GL_Type (TE) = GT,
          Inline;

   procedure Set_Is_Nonnative_Type    (TE : Entity_Id; B : Boolean := True)
     with Pre => Is_Type (TE), Post => Is_Nonnative_Type_N (TE) = B, Inline;

   procedure Set_Is_Being_Elaborated  (TE : Entity_Id; B : Boolean)
     with Pre  => Is_Type_Or_Void (TE), Post => Is_Being_Elaborated (TE) = B,
          Inline;

   procedure Set_TBAA                 (TE : Entity_Id; TBAA : Metadata_T)
     with Pre  => Is_Type_Or_Void (TE) and then Present (TBAA),
          Post => Get_TBAA_N (TE) = TBAA, Inline;

   procedure Set_TBAA_Info            (TE : Entity_Id; T : TBAA_Info_Id)
     with Pre  => Is_Type_Or_Void (TE) and then Present (T),
          Post => Get_TBAA_Info_N (TE) = T, Inline;

   procedure Set_Value_R              (VE : Entity_Id; VL : GL_Value)
     with Pre  => Present (VE)
                  and then (No (VL) or else No (Get_Value (VE))
                            or else Get_Value (VE) = VL or else No (VL)),
          Post => Get_Value (VE) = VL, Inline;

   procedure Set_SO_Ref               (N : N_Subexpr_Id; U : Dynamic_SO_Ref)
     with Pre  => Present (N) and then Present (U)
                  and then (No (Get_SO_Ref (N)) or else Get_SO_Ref (N) = U),
          Post => Get_SO_Ref (N) = U, Inline;

   procedure Set_Debug_Type           (TE : Entity_Id; DT : Metadata_T)
     with Pre  => Is_Type_Or_Void (TE)
                  and then (No (Get_Debug_Type_N (TE))
                              or else Get_Debug_Type_N (TE) = DT),
          Post => Get_Debug_Type_N (TE) = DT, Inline;

   procedure Set_Array_Info           (TE : Entity_Id; AI : Array_Info_Id)
     with Pre  => Is_Array_Type (TE)
                  and then (No (Get_Array_Info_N (TE))
                              or else Get_Array_Info_N (TE) = AI),
          Post => Get_Array_Info_N (TE) = AI, Inline;

   procedure Set_Orig_Array_Info       (TE : Entity_Id; AI : Array_Info_Id)
     with Pre  => Is_Packed_Array_Impl_Type (TE)
                  and then (No (Get_Orig_Array_Info_N (TE))
                              or else Get_Orig_Array_Info_N (TE) = AI),
          Post => Get_Orig_Array_Info_N (TE) = AI, Inline;

   procedure Set_Record_Info           (TE : Entity_Id; RI : Record_Info_Id)
     with Pre  => Is_Record_Type (TE)
                  and then (No (Get_Record_Info_N (TE))
                              or else Get_Record_Info_N (TE) = RI),
          Post => Get_Record_Info_N (TE) = RI, Inline;

   procedure Set_Field_Info            (VE : Entity_Id; FI : Field_Info_Id)
     with Pre  => Ekind (VE) in Record_Field_Kind
                  and then (No (Get_Field_Info (VE))
                              or else Get_Field_Info (VE) = FI),
          Post => Get_Field_Info (VE) = FI, Inline;

   procedure Set_Label_Info            (VE : Entity_Id; LI : Label_Info_Id)
     with Pre  => Present (VE)
                  and then (No (Get_Label_Info (VE))
                              or else Get_Label_Info (VE) = LI),
          Post => Get_Label_Info (VE) = LI, Inline;

   procedure Set_Subprogram_Type       (VE : Entity_Id; T : Type_T)
     with Pre  => Present (VE)
                  and then (No (Get_Subprogram_Type (VE))
                              or else Get_Subprogram_Type (VE) = T),
          Post => Get_Subprogram_Type (VE) = T, Inline;

   procedure Set_Flag1                 (VE : Entity_Id; F : Boolean)
     with Pre  => Present (VE),
          Post => Get_Flag1 (VE) = F, Inline;

end GNATLLVM.Environment;
