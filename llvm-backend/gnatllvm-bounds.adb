with Einfo; use Einfo;

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
        Env.Bld.I_Cmp
          ((if Is_Bound_Unsigned then Int_UGT else Int_SGT),
           Low_Bound, High_Bound, "is-array-empty");

   begin
      return Env.Bld.Build_Select
        (C_If   => Is_Empty,
         C_Then => Const_Null (Result_Type),
         C_Else =>
           Env.Bld.Add
             (Env.Bld.Sub (High_Bound, Low_Bound, ""),
              Const_Int (Result_Type, 1, Sign_Extend => False),
              ""),
         Name   => "");
   end Bounds_To_Length;

end GNATLLVM.Bounds;
