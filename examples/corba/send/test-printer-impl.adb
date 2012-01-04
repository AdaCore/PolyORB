------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    T E S T . P R I N T E R . I M P L                     --
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
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;

with Test.Printer.Skel;
pragma Warnings (Off, Test.Printer.Skel);

package body Test.Printer.Impl is

   Var_PrintString_Called : Natural := 0;
   Var_PrintLong_Called : Natural := 0;

   -----------------
   -- PrintString --
   -----------------

   procedure PrintString (Self : access Object; Mesg : CORBA.String) is
      pragma Unreferenced (Self);
   begin
      Ada.Text_IO.Put_Line
        ("Printing string: «" & CORBA.To_Standard_String (Mesg) & "»");
      Var_PrintString_Called := Var_PrintString_Called + 1;
   end PrintString;

   ---------------
   -- PrintLong --
   ---------------

   procedure PrintLong (Self : access Object; K : CORBA.Long) is
      pragma Unreferenced (Self);
   begin
      Ada.Text_IO.Put_Line ("Printing Long: " & CORBA.Long'Image (K));
      Var_PrintLong_Called := Var_PrintLong_Called + 1;
   end PrintLong;

   ----------------
   -- EchoString --
   ----------------

   function EchoString
     (Self : access Object;
      Mesg : CORBA.String)
     return CORBA.String
   is
      pragma Unreferenced (Self);

   begin
      Ada.Text_IO.Put_Line ("Echoing : "
                            & CORBA.To_Standard_String (Mesg));

      return Mesg;
   end EchoString;

   --------------
   -- EchoLong --
   --------------

   function EchoLong
     (Self : access Object;
      K    : CORBA.Long)
     return CORBA.Long
   is
      pragma Unreferenced (Self);

   begin
      Ada.Text_IO.Put_Line ("Echoing : " & CORBA.Long'Image (K));
      return K;
   end EchoLong;

   ------------------------
   -- PrintString_Called --
   ------------------------

   function PrintString_Called return Natural is
   begin
      return Var_Printstring_Called;
   end PrintString_Called;

   ----------------------
   -- PrintLong_Called --
   ----------------------

   function PrintLong_Called return Natural is
   begin
      return Var_PrintLong_Called;
   end PrintLong_Called;

end Test.Printer.Impl;
