with Stack;

function Main return Natural is
   S : Stack.Stack := Stack.New_Stack;
   A : Natural;
begin
   Stack.Append (S, 1);
   Stack.Append (S, 2);
   Stack.Append (S, 3);
   Stack.Append (S, 4);
   A := Stack.Pop (S);
   A := Stack.Pop (S);
   return Stack.Pop (S);
end;
