------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                 S E N D                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

with Ada.Command_Line;
with Ada.Text_IO;

with CORBA.ORB;

with Test.Printer;
with Test.Controller;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

with PolyORB.Utils.Report;

procedure Send is

   use Ada.Command_Line;
   use Ada.Text_IO;

   use PolyORB.Utils.Report;

   Ok : Boolean := False;

begin
   CORBA.ORB.Initialize ("ORB");

   New_Test ("CORBA/MIOP");

   --  Parse command line

   if Argument_Count /= 1 then
      Put_Line ("usage : ./send <IOR|corbaloc>");
      return;
   end if;

   declare
      Sent_Msg : constant Standard.String
        := "Hello Multicast world !";

      The_Controller : Test.Controller.Ref;
      Printer : Test.Printer.Ref;

   begin
      CORBA.ORB.String_To_Object
        (CORBA.To_CORBA_String (Ada.Command_Line.Argument (1)),
         The_Controller);

      Printer := Test.Controller.Get_Printer (The_Controller);

      --  Check reference is correct

      if Test.Printer.Is_Nil (Printer) then
         Put_Line ("main : cannot invoke on a nil reference");
         return;
      end if;

      --  Send messages

      Test.Printer.printString
        (Printer,
         CORBA.To_CORBA_String (Sent_Msg));

      Output ("Sending CORBA.String", True);

      Test.Printer.printLong (Printer, CORBA.Long (1234));

      Output ("Sending CORBA.Long", True);

      Output ("Previous tests went OK on the server side",
              Test.Controller.Test_OK (The_Controller));

      declare
         Result : CORBA.String;
         pragma Unreferenced (Result);

      begin
         Result := Test.Printer.EchoString
           (Printer, CORBA.To_CORBA_String (Sent_Msg));
         Output ("Calling function with return value raised an exception",
                 False);
      exception
         when others =>
         Output ("Calling function with return value raised an exception",
                 True);
      end;

      begin
         Test.Controller.StopServer (The_Controller);
         Ok := True;
      exception
         when others =>
            Ok := False;
            raise;
      end;

      Output ("Shut down server(s)", Ok);

   exception
      when E : CORBA.Transient =>
         declare
            Memb : CORBA.System_Exception_Members;
         begin
            CORBA.Get_Members (E, Memb);
            Put ("received exception transient, minor");
            Put (CORBA.Unsigned_Long'Image (Memb.Minor));
            Put (", completion status: ");
            Put_Line (CORBA.Completion_Status'Image (Memb.Completed));
         end;
   end;

   End_Report;
end Send;
