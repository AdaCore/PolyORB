with Ada.Unchecked_Conversion;

package body Utils is

   ---------
   -- Img --
   ---------

   function Img (N : Natural) return String is
      S : constant String := Natural'Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end Img;

   ---------
   -- Img --
   ---------

   function Img (N : Node_Id) return String is
   begin
      if N = No_Node then
         return "No_Node";
      else
         return Img (Natural (N));
      end if;
   end Img;

   ---------
   -- Img --
   ---------

   function Img (N : Node_Kind) return String is
   begin
      return Node_Kind'Image (N);
   end Img;

   ---------
   -- Img --
   ---------

   function Img (B : Boolean) return String is
   begin
      if B then
         return "True";
      else
         return "False";
      end if;
   end Img;

   ---------
   -- Img --
   ---------

   function Img (A : System.Address) return String is

      type U_32 is mod 2 ** 32;
      for U_32'Size use 32;

      function To_U_32 is
         new Ada.Unchecked_Conversion (System.Address, U_32);

      Hex_Digits : constant array
        (U_32 range 0 .. 15) of Character
        := "0123456789abcdef";

      Integer_Address : U_32
        := To_U_32 (A);

      Result : String (1 .. 8);
   begin
      for I in reverse Result'Range loop
         Result (I) := Hex_Digits
           (Integer_Address mod 16);
         Integer_Address := Integer_Address / 16;
      end loop;
      return Result;
   end Img;

end Utils;
