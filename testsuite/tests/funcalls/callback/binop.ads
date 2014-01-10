package BinOp is

   type Op is (Sum, Sub, Mul, Div);

   function Eval (O : Op; L, R : Integer) return Integer;

end BinOp;
