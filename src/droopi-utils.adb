package body Droopi.Utils is

   use Ada.Streams;

   ------------------------
   -- Local declarations --
   ------------------------

   Hex : constant array (16#0# .. 16#f#) of Character
     := "0123456789abcdef";

   Hex_Val : constant array (Character) of Integer
     := ('0' => 0,
         '1' => 1,
         '2' => 2,
         '3' => 3,
         '4' => 4,
         '5' => 5,
         '6' => 6,
         '7' => 7,
         '8' => 8,
         '9' => 9,
         'A' => 10,
         'a' => 10,
         'B' => 11,
         'b' => 11,
         'C' => 12,
         'c' => 12,
         'D' => 13,
         'd' => 13,
         'E' => 14,
         'e' => 14,
         'F' => 15,
         'f' => 15,
         others => -1);

   function Hex_Value (C : Character) return Integer;
   --  Self-explanatory.

   ---------------
   -- Hex_Value --
   ---------------

   function Hex_Value (C : Character) return Integer is
      V : constant Integer := Hex_Val (C);
   begin
      if V = -1 then
         raise Constraint_Error;
      else
         return V;
      end if;
   end Hex_Value;

   ---------------
   -- To_String --
   ---------------

   function To_String (A : Stream_Element_Array) return String
   is
      S : String (1 .. 2 * A'Length);
   begin
      for I in A'Range loop
         S (S'First + 2 * Integer (I - A'First))
           := Hex (Integer (A (I)) / 16);
         S (S'First + 2 * Integer (I - A'First) + 1)
           := Hex (Integer (A (I)) mod 16);
      end loop;
      return S;
   end To_String;

   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------

   function To_Stream_Element_Array (S : String) return Stream_Element_Array
   is
      A : Stream_Element_Array (1 .. S'Length / 2);
   begin
      for I in A'Range loop
         A (I) :=
           Stream_Element
           (Hex_Value (S (S'First + 2 * Integer (I - A'First))) * 16
            + Hex_Value (S (S'First + 2 * Integer (I - A'First) + 1)));
      end loop;
      return A;
   end To_Stream_Element_Array;

end Droopi.Utils;
