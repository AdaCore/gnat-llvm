with Types; use Types;

with LLVM.Core; use LLVM.Core;
with LLVM.Types; use LLVM.Types;

with Atree; use Atree;
with Einfo; use Einfo;

with GNATLLVM.Environment;  use GNATLLVM.Environment;
with GNATLLVM.Nested_Subps; use GNATLLVM.Nested_Subps;
with GNATLLVM.Utils; use GNATLLVM.Utils;
with Interfaces.C;
with Get_Targ; use Get_Targ;

package GNATLLVM.Types is

   pragma Annotate (Xcov, Exempt_On, "Defensive programming");
   procedure Register_Builtin_Types (Env : Environ);

   function Create_Access_Type
     (Env : Environ; TE : Entity_Id) return Type_T
     with Pre => Is_Type (TE);
   --  Function that creates the access type for a corresponding type. Since
   --  access types are not just pointers, this is the abstraction bridge
   --  between the two. For the moment, it handles array accesses and thin
   --  (normal) accesses.

   function Create_Array_Thin_Pointer_Type
     (Env        : Environ;
      Array_Type : Entity_Id) return Type_T;
   --  Return the type used to store thin pointers to Array_Type

   function Create_Array_Fat_Pointer_Type
     (Env        : Environ;
      Array_Type : Entity_Id) return Type_T;
   --  Return the type used to store fat pointers to Array_Type

   function Create_Array_Bounds_Type
     (Env             : Environ;
      Array_Type_Node : Entity_Id) return Type_T;
   --  Helper that returns the type used to store array bounds. This is a
   --  structure that that follows the following pattern: { LB0, UB0, LB1,
   --  UB1, ... }

   function Create_Subprogram_Type_From_Spec
     (Env       : Environ;
      Subp_Spec : Node_Id) return Type_T;

   function Create_Subprogram_Type_From_Entity
     (Env           : Environ;
      Subp_Type_Ent : Entity_Id;
      Takes_S_Link  : Boolean) return Type_T;

   function Create_Type (Env : Environ; TE : Entity_Id) return Type_T
     with Pre => Is_Type (TE);

   procedure Create_Discrete_Type
     (Env       : Environ;
      TE        : Entity_Id;
      TL        : out Type_T;
      Low, High : out Value_T)
     with Pre => Ekind (TE) in Discrete_Kind;

   function Create_Static_Link_Type
     (Env         : Environ;
      S_Link_Desc : Static_Link_Descriptor) return Type_T;
   --  Return an LLVM type for the structure used to implement the statick
   --  link.

   function Int_Ty (Num_Bits : Natural) return Type_T;
   function Fn_Ty (Param_Ty : Type_Array; Ret_Ty : Type_T) return Type_T;

   function Get_Innermost_Component_Type
     (Env : Environ; N : Entity_Id) return Type_T;

   function Get_Address_Type return Type_T;
   pragma Annotate (Xcov, Exempt_Off, "Defensive programming");

   function Int_Ptr_Type return Type_T is
      (Int_Type (Interfaces.C.unsigned (Get_Pointer_Size)));

end GNATLLVM.Types;
