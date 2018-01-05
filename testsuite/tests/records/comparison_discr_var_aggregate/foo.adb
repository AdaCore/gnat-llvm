with Interfaces; use Interfaces;

function Foo (First, Second : Unsigned_32; Equality : Boolean) return Boolean
is

   Bits_Per_Arg   : constant Natural := 4;
   First_Negative : constant Unsigned_32 := 2 ** (Bits_Per_Arg - 1);

   function Decode (Arg : Unsigned_32; N : Natural) return Integer is
      Tmp : constant Unsigned_32 :=
         Shift_Right (Arg, N * 4) and (2 ** Bits_Per_Arg - 1);
   begin
      if Tmp < First_Negative then
         return Integer (Tmp);
      else
         return -Integer (2 ** Bits_Per_Arg - Tmp);
      end if;
   end Decode;

   type A_Type is array (Integer range <>) of Integer;
   type R_Type (L : Integer) is record
      A : A_Type (1 .. L);
      B : Integer;
   end record;

   A : constant Integer := Decode (First, 2);
   B : constant Integer := Decode (First, 1);
   C : constant Integer := Decode (First, 0);
   D : constant Integer := Decode (Second, 2);
   E : constant Integer := Decode (Second, 1);
   F : constant Integer := Decode (Second, 0);

   R1 : constant R_Type :=
     (L => A,
      A => (others => B),
      B => C);
begin
   if Equality then
      return R1 = (L => D, A => (others => E), B => F);
   else
      return R1 /= (L => D, A => (others => E), B => F);
   end if;
end Foo;
