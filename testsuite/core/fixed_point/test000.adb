------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
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
