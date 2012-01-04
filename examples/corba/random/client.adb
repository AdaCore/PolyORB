------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
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

--   Random client.

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with CORBA; use CORBA;
with CORBA.ORB;
with Random;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

with PolyORB.Utils.Report;

procedure Client is
   use PolyORB.Utils.Report;

   IOR : CORBA.String;
   myRandom : Random.Ref;
   Result : CORBA.Long;

begin
   New_Test ("CORBA Random");

   CORBA.ORB.Initialize ("ORB");
   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   --  transforms the Ada string into CORBA.String
   IOR := CORBA.To_CORBA_String (Ada.Command_Line.Argument (1));

   --  getting the CORBA.Object
   CORBA.ORB.String_To_Object (IOR, myRandom);

   --  checking if it worked
   if Random.Is_Nil (myRandom) then
      Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   Put_Line ("Here are some true random numbers:");

   for I in 1 .. 10 loop
      Result := Random.lrand48 (myRandom);
      Put (CORBA.Long'Image (Result) & " ");
   end loop;
   New_Line;

   End_Report;
exception
   when E : CORBA.Transient =>
      declare
         Memb : System_Exception_Members;
      begin
         Get_Members (E, Memb);
         Put ("received exception transient, minor");
         Put (Unsigned_Long'Image (Memb.Minor));
         Put (", completion status: ");
         Put_Line (Completion_Status'Image (Memb.Completed));
         End_Report;
      end;
end Client;
