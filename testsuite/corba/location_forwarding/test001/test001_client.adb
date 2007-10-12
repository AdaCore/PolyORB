------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       T E S T 0 0 1 _ C L I E N T                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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
