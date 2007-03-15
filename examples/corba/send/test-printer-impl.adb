------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    T E S T . P R I N T E R . I M P L                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2004 Free Software Foundation, Inc.           --
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

with Ada.Text_IO;

with Test.Printer.Skel;
pragma Warnings (Off, Test.Printer.Skel);

package body Test.Printer.Impl is

   -----------------
   -- PrintString --
   -----------------

   procedure PrintString
     (Self : access Object;
      Mesg : in     CORBA.String)
   is
      pragma Unreferenced (Self);
   begin
      Ada.Text_IO.Put_Line
        ("Printing string: «" & CORBA.To_Standard_String (Mesg)
         & "»");
   end PrintString;

   ---------------
   -- PrintLong --
   ---------------

   procedure PrintLong
     (Self : access Object;
      K    : in     CORBA.Long)
   is
      pragma Unreferenced (Self);

   begin
      Ada.Text_IO.Put_Line ("Printing Long: " & CORBA.Long'Image (K));
   end PrintLong;

   ----------------
   -- EchoString --
   ----------------

   function EchoString
     (Self : access Object;
      Mesg : in     CORBA.String)
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
      K    : in     CORBA.Long)
     return CORBA.Long
   is
      pragma Unreferenced (Self);

   begin
      Ada.Text_IO.Put_Line ("Echoing : " & CORBA.Long'Image (K));
      return K;
   end EchoLong;

end Test.Printer.Impl;
