function Foo (I : Integer) return Integer is

   generic
      type Iterator_Type is private;
      with procedure Init (It : in out Iterator_Type);
      with function Has_Next (It : Iterator_Type) return Boolean;
      with procedure Next (It : in out Iterator_Type; I : out Integer);
   function Sequence_Sum return Integer;

   function Sequence_Sum return Integer is
      It     : Iterator_Type;
      I      : Integer;
      Result : Integer := 0;
   begin
      Init (It);
      while Has_Next (It) loop
         Next (It, I);
         Result := Result + I;
      end loop;
      return Result;
   end Sequence_Sum;

   procedure Init (It : in out Integer) is
   begin
      It := I;
   end Init;

   function Has_Next (It : Integer) return Boolean is
   begin
      return It > 0;
   end Has_Next;

   procedure Next (It : in out Integer; I : out Integer) is
   begin
      I := It * It;
      It := It - 1;
   end Next;

   function Square_Sum is new Sequence_Sum
     (Iterator_Type => Integer,
      Init          => Init,
      Has_Next      => Has_Next,
      Next          => Next);

begin
   return Square_Sum;
end Foo;
