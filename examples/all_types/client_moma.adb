------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          C L I E N T _ M O M A                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Testing MOMA client.

--  $Id$

with Ada.Command_Line;
with Ada.Text_IO;

with PolyORB.Setup.No_Tasking_Server;
pragma Elaborate_All (PolyORB.Setup.No_Tasking_Server);
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);
--  XXX this package should be renamed to PolyORB.Setup.No_Tasking_Node ...

--  XXX do not change Tasking model for now, otherwise there is a risk
--  of a race condition between producer and consumer ...

with MOMA.Connection_Factories.Queues;
with MOMA.Connections.Queues;
with MOMA.Connections;
with MOMA.Message_Producers.Queues;
with MOMA.Messages;
with MOMA.Messages.MTexts;
with MOMA.Messages.MBytes;
with MOMA.Types;
with PolyORB.Types;

with Report;

procedure Client_MOMA is
pragma Warnings (Off);

   use Ada.Command_Line;
   use Ada.Text_IO;

   use MOMA.Connection_Factories.Queues;
   use MOMA.Connections;
   use MOMA.Message_Producers.Queues;
   use MOMA.Messages;
   use MOMA.Messages.MTexts;
   use MOMA.Messages.MBytes;
   use MOMA.Types;
   use PolyORB.Types;
   use Report;

   MOMA_Queue         : MOMA.Connections.Queues.Queue;

   Ok : Boolean;

   type Scenario_T is (Full, Stor, Retr);
   Scenario : Scenario_T := Full;

   -----------------------
   -- Text message test --
   -----------------------

   procedure Test_MText;

   procedure Test_MText
   is
      MText_Message_Sent : MOMA.Messages.MTexts.MText;
      MText_Message_Rcvd : MOMA.Messages.MTexts.MText;
   begin
      --  Create new Text Message
      MText_Message_Sent := Create_Text_Message;
      Set_Text (MText_Message_Sent, To_MOMA_String ("Hi MOM !"));

      --  Get Text Message
      declare
         MOMA_Message_Temp : MOMA.Messages.Message'Class
           := Send_Receive (MOMA_Queue, "echoString", MText_Message_Sent);
      begin
         if MOMA_Message_Temp in MOMA.Messages.MTexts.MText then
            MText_Message_Rcvd :=
              MOMA.Messages.MTexts.MText (MOMA_Message_Temp);
         else
            raise Program_Error;
         end if;
      end;

      --  Print results
      Ok := Get_Text (MText_Message_Sent) = Get_Text (MText_Message_Rcvd);
      Output ("Testing Text Message ", Ok);

   end Test_MText;

   -----------------------
   -- Byte Message Test --
   -----------------------

   procedure Test_MByte;

   procedure Test_MByte
   is
      MByte_Message_Sent : MOMA.Messages.MBytes.MByte;
      MByte_Message_Rcvd : MOMA.Messages.MBytes.MByte;

      procedure Send_Receive_MByte (Operation_Name : String);

      procedure Send_Receive_MByte (Operation_Name : String) is
      begin

         --  Get byte Message
         declare
            MOMA_Message_Temp : MOMA.Messages.Message'Class
              := Send_Receive (MOMA_Queue, Operation_Name, MByte_Message_Sent);
         begin
            if MOMA_Message_Temp in MOMA.Messages.MBytes.MByte then
               MByte_Message_Rcvd :=
                 MOMA.Messages.MBytes.MByte (MOMA_Message_Temp);
            else
               raise Program_Error;
            end if;
         end;
      end Send_Receive_MByte;

   begin

      --  Create new Byte Message
      MByte_Message_Sent := Create_Byte_Message;

      --  Byte/Boolean Test
      Set_Boolean (MByte_Message_Sent, MOMA.Types.Boolean (True));
      Send_Receive_MByte ("echoBoolean");
      Ok := Get_Boolean (MByte_Message_Sent)
        = Get_Boolean (MByte_Message_Rcvd);
      Output ("Testing Byte/Boolean Message ", Ok);

      --  Byte/Byte Test
      Set_Byte (MByte_Message_Sent, MOMA.Types.Byte (42));
      Send_Receive_MByte ("echoOctet");
      Ok := Get_Byte (MByte_Message_Sent) = Get_Byte (MByte_Message_Rcvd);
      Output ("Testing Byte/Byte Message ", Ok);

      --  Byte/Char Test
      Set_Char (MByte_Message_Sent,
                MOMA.Types.Char (Character'('A')));
      Send_Receive_MByte ("echoChar");
      Ok := Get_Char (MByte_Message_Sent) = Get_Char (MByte_Message_Rcvd);
      Output ("Testing Byte/Char Message ", Ok);

      --  Byte/Double Test
      Set_Double (MByte_Message_Sent, MOMA.Types.Double (42.0));
      Send_Receive_MByte ("echoDouble");
      Ok := Get_Double (MByte_Message_Sent)
        = Get_Double (MByte_Message_Rcvd);
      Output ("Testing Byte/Double Message ", Ok);

      --  Byte/Float Test
      Set_Float (MByte_Message_Sent, MOMA.Types.Float (42.0));
      Send_Receive_MByte ("echoFloat");
      Ok := Get_Float (MByte_Message_Sent) = Get_Float (MByte_Message_Rcvd);
      Output ("Testing Byte/Float Message ", Ok);

      --  Byte/Short Test
      Set_Short (MByte_Message_Sent, MOMA.Types.Short (3));
      Send_Receive_MByte ("echoShort");
      Ok := Get_Short (MByte_Message_Sent) = Get_Short (MByte_Message_Rcvd);
      Output ("Testing Byte/Short Message ", Ok);

      --  Byte/Long Test
      Set_Long (MByte_Message_Sent, MOMA.Types.Long (21));
      Send_Receive_MByte ("echoLong");
      Ok := Get_Long (MByte_Message_Sent) = Get_Long (MByte_Message_Rcvd);
      Output ("Testing Byte/Long Message ", Ok);

      --  Byte/Unsigned_Long Test
      Set_Unsigned_Long (MByte_Message_Sent, MOMA.Types.Unsigned_Long (12345));
      Send_Receive_MByte ("echoULong");
      Ok := Get_Unsigned_Long (MByte_Message_Sent)
        = Get_Unsigned_Long (MByte_Message_Rcvd);
      Output ("Testing Byte/Unsigned_Long Message ", Ok);

      --  Byte/Unsigned_Short Test
      Set_Unsigned_Short (MByte_Message_Sent, MOMA.Types.Unsigned_Short (123));
      Send_Receive_MByte ("echoUShort");
      Ok := Get_Unsigned_Short (MByte_Message_Sent)
        = Get_Unsigned_Short (MByte_Message_Rcvd);
      Output ("Testing Byte/Unsigned_Short Message ", Ok);

   end Test_MByte;

   --------------------
   -- Main procedure --
   --------------------

begin
   --  Argument check
   if Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   --  Create Queue using Queue Connection Factory
   MOMA_Queue := Create (To_MOMA_String (Ada.Command_Line.Argument (1)));

   Put_Line ("Ready to send messages !");

   --  Testing MText.

   Test_MText;

   --  Testing MByte.

   Test_MByte;

   --  End of File
   --  Put_Line ("waiting");
   --  delay 10.0;

   --  XXX should destroy all structures here !
pragma Warnings (On);
end Client_MOMA;
