------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                 S E N D                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
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
