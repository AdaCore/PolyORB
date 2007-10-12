------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;
with PolyORB.Fixed_Point; use PolyORB.Fixed_Point;
with Ada.Text_IO; use Ada.Text_IO;
with PolyORB.Utils.Report; use PolyORB.Utils.Report;

procedure Test000 is
   type Megabucks is delta 0.01 digits 15;
   package Megabucks_Conv is
     new PolyORB.Fixed_Point.Fixed_Point_Conversion (Megabucks);
   use Megabucks_Conv;

   Values : constant array (Integer range <>) of Megabucks :=
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
