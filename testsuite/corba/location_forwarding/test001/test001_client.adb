------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       T E S T 0 0 1 _ C L I E N T                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with Ada.Command_Line;

with CORBA.ORB;
with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);
with PolyORB.Utils.Report;

with Test_Interface;

procedure Test001_Client is
   Object : Test_Interface.Ref;
begin
   PolyORB.Utils.Report.New_Test
    ("GIOP ServantActivator Location Forwarding");

   CORBA.ORB.Initialize ("ORB");

   CORBA.ORB.String_To_Object
    (CORBA.To_CORBA_String (Ada.Command_Line.Argument (1)), Object);

   for J in 1 .. 10 loop
      declare
         use type CORBA.String;
      begin
         if Test_Interface.Name (Object) = "Hello, world!" then
            PolyORB.Utils.Report.Output
             ("ServantManager location forwarding (pass"
                & Integer'Image (J) & ")",
              True);
         else
            PolyORB.Utils.Report.Output
             ("ServantManager location forwarding (pass"
                & Integer'Image (J) & ")",
              False);
         end if;
      exception
         when others =>
            PolyORB.Utils.Report.Output
             ("ServantManager location forwarding (pass"
                & Integer'Image (J) & ")",
              False);
      end;
   end loop;

   CORBA.ORB.Shutdown (False);

   PolyORB.Utils.Report.End_Report;
end Test001_Client;
