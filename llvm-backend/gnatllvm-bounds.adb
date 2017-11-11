with Einfo; use Einfo;
with LLVM.Core; use LLVM.Core;

package body GNATLLVM.Bounds is

   ----------------------
   -- Bounds_To_Length --
   ----------------------

   function Bounds_To_Length
     (Env                   : Environ;
      Low_Bound, High_Bound : Value_T;
      Bounds_Type           : Entity_Id) return Value_T
   is
      Result_Type : constant Type_T := Type_Of (Low_Bound);

      Is_Bound_Unsigned  : constant Boolean :=
        Is_Unsigned_Type (Bounds_Type);
      Is_Empty         : constant Value_T :=
        I_Cmp
          (Env.Bld,
           (if Is_Bound_Unsigned then Int_UGT else Int_SGT),
           Low_Bound, High_Bound, "is-array-empty");

   begin
      return Build_Select
        (Env.Bld,
         C_If   => Is_Empty,
         C_Then => Const_Null (Result_Type),
         C_Else =>
           Add
             (Env.Bld,
              Sub (Env.Bld, High_Bound, Low_Bound, ""),
              Const_Int (Result_Type, 1, Sign_Extend => LLVM.Types.False),
              ""),
         Name   => "");
   end Bounds_To_Length;

end GNATLLVM.Bounds;
