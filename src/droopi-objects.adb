--  Root type for concrete object implementations (servants).

--  $Id$

with Ada.Unchecked_Deallocation;

package body Droopi.Objects is

   procedure Free (X : in out Object_Id_Access)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Object_Id, Object_Id_Access);
   begin
      Free (X);
   end Free;

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

   function To_String (Oid : Object_Id) return String
   is
      S : String (1 .. 2 * Oid'Length);
   begin
      for I in Oid'Range loop
         S (S'First + 2 * Integer (I - Oid'First))
           := Hex (Integer (Oid (I)) / 16);
         S (S'First + 2 * Integer (I - Oid'First) + 1)
           := Hex (Integer (Oid (I)) mod 16);
      end loop;
      return S;
   end To_String;

   function Hex_Value (C : Character) return Integer;

   function Hex_Value (C : Character) return Integer is
      V : constant Integer := Hex_Val (C);
   begin
      if V = -1 then
         raise Constraint_Error;
      else
         return V;
      end if;
   end Hex_Value;

   function To_Oid (S : String) return Object_Id
   is
      Oid : Object_Id (1 .. S'Length / 2);
   begin
      for I in Oid'Range loop
         Oid (I) :=
           Stream_Element
           (Hex_Value (S (S'First + 2 * Integer (I - Oid'First))) * 16
            + Hex_Value (S (S'First + 2 * Integer (I - Oid'First) + 1)));
      end loop;
      return Oid;
   end To_Oid;

   function Image (Oid : Object_Id) return String renames To_String;

end Droopi.Objects;
