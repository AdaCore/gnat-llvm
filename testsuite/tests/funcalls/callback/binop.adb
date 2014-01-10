package body BinOp is

   type Callback is access function (L, R : Integer) return Integer;
   type Callback_Array is array (Op) of Callback;

   function Eval_Sum (L, R : Integer) return Integer;
   function Eval_Sub (L, R : Integer) return Integer;
   function Eval_Mul (L, R : Integer) return Integer;
   function Eval_Div (L, R : Integer) return Integer;

   function Eval (O : Op; L, R : Integer) return Integer is
      Callbacks : constant Callback_Array :=
         (Eval_Sum'Access,
          Eval_Sub'Access,
          Eval_Mul'Access,
          Eval_Div'Access);
   begin
      return Callbacks (O).all (L, R);
   end Eval;

   function Eval_Sum (L, R : Integer) return Integer is
   begin
      return L + R;
   end Eval_Sum;

   function Eval_Sub (L, R : Integer) return Integer is
   begin
      return L - R;
   end Eval_Sub;

   function Eval_Mul (L, R : Integer) return Integer is
   begin
      return L * R;
   end Eval_Mul;

   function Eval_Div (L, R : Integer) return Integer is
   begin
      return L / R;
   end Eval_Div;

end BinOp;
