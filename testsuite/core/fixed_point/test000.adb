with Ada.Exceptions;
with PolyORB.Fixed_Point; use PolyORB.Fixed_Point;
with Ada.Text_IO; use Ada.Text_IO;
with PolyORB.Report; use PolyORB.Report;

procedure Test000 is
   type Megabucks is delta 0.01 digits 15;
   package Megabucks_Conv is
     new PolyORB.Fixed_Point.Fixed_Point_Conversion (Megabucks);
   use Megabucks_Conv;

   Values : array (Integer range <>) of Megabucks :=
     (0.0, 0.01, 0.05, 1.23, -1.0, 12345.67, 123456.78, -0.01,
      9999.99, 99999.99);

   Hex_Digit : constant array (Nibble) of Character := "0123456789abcdef";
begin
   for I in Values'Range loop
      begin
         declare
            R : constant Nibbles := Fixed_To_Nibbles (Values (I));
         begin
            Put (Values (I)'Img & " ->");
            for J in R'Range loop
               Put (" " & Hex_Digit (R (J)));
            end loop;
            Put_Line ("");
            Output
              ("test " & Megabucks'Image (Values (I)),
               Values (I) = Nibbles_To_Fixed (R));
         end;
      exception
         when E : others =>
            Put_Line (Ada.Exceptions.Exception_Information (E));
            Output ("test " & Megabucks'Image (Values (I)), False);
      end;
   end loop;
   End_Report;
end Test000;
