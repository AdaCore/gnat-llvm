with Types; use Types;

with LLVM.Core; use LLVM.Core;

with GNATLLVM.Environment; use GNATLLVM.Environment;

package GNATLLVM.Types is

   type Type_Array is array (Nat range <>) of Type_T;

   procedure Register_Builtin_Types (Env : Environ);

   function Create_Subprogram_Type
     (Env : Environ; Subp_Spec : Node_Id) return Type_T;

   function Create_Type (Env : Environ; Type_Node : Node_Id) return Type_T;

   procedure Create_Discrete_Type
     (Env       : Environ;
      TE        : Entity_Id;
      TL        : out Type_T;
      Low, High : out Value_T);

   function Int_Ty (Num_Bits : Natural) return Type_T;
   function Fn_Ty (Param_Ty : Type_Array; Ret_Ty : Type_T) return Type_T;

   function Get_Innermost_Component_Type
     (Env : Environ; N : Entity_Id) return Type_T;

   function Get_Binary_Size (N : Natural) return Natural;
   --  Return the number of bits required to store N

end GNATLLVM.Types;
