------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
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
--  XXX do not change Tasking model for now, otherwise there is a risk
--  of a race condition between producer and consumer ...

with PolyORB.Services.Naming.Tools;

with MOMA.Connection_Factories.Queues;
with MOMA.Connections.Queues;
with MOMA.Connections;
with MOMA.Sessions.Queues;
with MOMA.Destinations;

with MOMA.Message_Producers.Queues;
with MOMA.Message_Consumers.Queues;

with MOMA.Messages;
with MOMA.Messages.MAnys;
with MOMA.Messages.MBytes;
with MOMA.Messages.MMaps;
with MOMA.Messages.MTexts;

with MOMA.Types;

with PolyORB.Any;
with PolyORB.References;
with PolyORB.References.IOR;
with PolyORB.Types;

with Report;

procedure Client is

   use Ada.Command_Line;
   use Ada.Text_IO;

   use MOMA.Connection_Factories.Queues;
   use MOMA.Sessions.Queues;
   use MOMA.Connections;
   use MOMA.Destinations;
   use MOMA.Message_Producers.Queues;
   use MOMA.Message_Consumers.Queues;
   use MOMA.Messages;
   use MOMA.Types;

   use PolyORB.Services.Naming.Tools;
   use PolyORB.Types;

   use Report;

   Pool_Ref           : PolyORB.References.Ref;
   MOMA_Factory       : Connection_Factory_Queue;
   MOMA_Connection    : MOMA.Connections.Queues.Queue;
   MOMA_Session       : MOMA.Sessions.Queues.Queue;
   MOMA_Destination   : MOMA.Destinations.Destination;
   MOMA_Producer      : MOMA.Message_Producers.Queues.Queue;
   MOMA_Consumer      : MOMA.Message_Consumers.Queues.Queue;

   Ok : Boolean;

   type Scenario_T is (Full, Stor, Retr);
   Scenario : Scenario_T;

   ----------------------
   -- Any Message Test --
   ----------------------

   procedure Test_MAny;

   procedure Test_MAny
   is
      use MOMA.Messages.MAnys;
      use PolyORB.Any;

      MAny_Message_Sent : MOMA.Messages.MAnys.MAny;
      MAny_Message_Rcvd : MOMA.Messages.MAnys.MAny;

   begin
      --  Create new Any Message
      MAny_Message_Sent := Create_Any_Message;
      Set_Any (MAny_Message_Sent, To_Any (To_MOMA_String ("Hi MOM !")));

      if Scenario in Full .. Stor then
         --  Send Any Message
         Send (MOMA_Producer, MAny_Message_Sent);
      end if;

      if Scenario = Full or Scenario = Retr then
         --  Get Any Message
         declare
            MOMA_Message_Temp : MOMA.Messages.Message'Class
              := Receive (MOMA_Consumer);
         begin
            if MOMA_Message_Temp in MOMA.Messages.MAnys.MAny then
               MAny_Message_Rcvd :=
                 MOMA.Messages.MAnys.MAny (MOMA_Message_Temp);
            else
               raise Program_Error;
            end if;
         end;

         Ok := Get_Payload (MAny_Message_Sent)
           = Get_Payload (MAny_Message_Rcvd);
         Output ("Testing Any Message ", Ok);
      end if;

   end Test_MAny;

   -----------------------
   -- Byte Message Test --
   -----------------------

   procedure Test_MByte;

   procedure Test_MByte
   is
      use MOMA.Messages.MBytes;
      use PolyORB.Any;

      MByte_Message_Sent : MOMA.Messages.MBytes.MByte;
      MByte_Message_Rcvd : MOMA.Messages.MBytes.MByte;

      procedure Send_Receive_MByte (Test_Name : String);

      procedure Send_Receive_MByte (Test_Name : String) is
      begin

         if Scenario in Full .. Stor then
            --  Send Byte message.
            Send (MOMA_Producer, MByte_Message_Sent);
         end if;

         if Scenario = Full or Scenario = Retr then
            --  Get Byte Message.
            declare
               MOMA_Message_Temp : MOMA.Messages.Message'Class
                 := Receive (MOMA_Consumer);
            begin
               if MOMA_Message_Temp in MOMA.Messages.MBytes.MByte then
                  MByte_Message_Rcvd :=
                    MOMA.Messages.MBytes.MByte (MOMA_Message_Temp);
               else
                  raise Program_Error;
               end if;
            end;
         end if;

         if Scenario /= Stor then
            --  Print result.
            Ok := Get_Payload (MByte_Message_Sent)
              = Get_Payload (MByte_Message_Rcvd);
            Output ("Testing " & Test_Name & " Message ", Ok);
         end if;
      end Send_Receive_MByte;

      use PolyORB.Any;

   begin

      --  Create new Byte Message
      MByte_Message_Sent := Create_Byte_Message;

      --  Byte/Boolean Test
      Set_Boolean (MByte_Message_Sent, MOMA.Types.Boolean (True));
      Send_Receive_MByte ("Byte/Boolean");

      --  Byte/Byte Test
      Set_Byte (MByte_Message_Sent, MOMA.Types.Byte (42));
      Send_Receive_MByte ("Byte/Byte");

      --  Byte/Char Test
      Set_Char (MByte_Message_Sent,
                MOMA.Types.Char (Character'('A')));
      Send_Receive_MByte ("Byte/Char");

      --  Byte/Double Test
      Set_Double (MByte_Message_Sent, MOMA.Types.Double (42.0));
      Send_Receive_MByte ("Byte/Double");

      --  Byte/Float Test
      Set_Float (MByte_Message_Sent, MOMA.Types.Float (42.0));
      Send_Receive_MByte ("Byte/Float");

      --  Byte/Short Test
      Set_Short (MByte_Message_Sent, MOMA.Types.Short (3));
      Send_Receive_MByte ("Byte/Short");

      --  Byte/Long Test
      Set_Long (MByte_Message_Sent, MOMA.Types.Long (21));
      Send_Receive_MByte ("Byte/Long");

      --  Byte/Unsigned_Long Test
      Set_Unsigned_Long (MByte_Message_Sent, MOMA.Types.Unsigned_Long (12345));
      Send_Receive_MByte ("Byte/Unsigned_Long");

      --  Byte/Unsigned_Short Test
      Set_Unsigned_Short (MByte_Message_Sent, MOMA.Types.Unsigned_Short (123));
      Send_Receive_MByte ("Byte/Unsigned_Short");

   end Test_MByte;

   ----------------------
   -- Map Message Test --
   ----------------------

   procedure Test_MMap;

   procedure Test_MMap
   is
      use MOMA.Messages.MMaps;
      use PolyORB.Any;

      MMap_Message_Sent : MOMA.Messages.MMaps.MMap;
      MMap_Message_Rcvd : MOMA.Messages.MMaps.MMap;

      Element_1 : Map_Element;
      Element_2 : Map_Element;
      My_Map    : Map;
   begin
      Element_1 := (Name  => To_MOMA_String ("name"),
                    Value => To_Any (To_MOMA_String ("John Doe")));
      Element_2 := (Name  => To_MOMA_String ("age"),
                    Value => To_Any (MOMA.Types.Short (42)));

      Append (My_Map, Element_1);
      Append (My_Map, Element_2);

      --  Create new Map Message
      MMap_Message_Sent := Create_Map_Message;
      Set_Map (MMap_Message_Sent, My_Map);

      if Scenario in Full .. Stor then
         --  Send Map Message
         Send (MOMA_Producer, MMap_Message_Sent);
      end if;

      if Scenario = Full or Scenario = Retr then
         --  Get Map Message
         declare
            MOMA_Message_Temp : MOMA.Messages.Message'Class
              := Receive (MOMA_Consumer);
         begin
            if MOMA_Message_Temp in MOMA.Messages.MMaps.MMap then
               MMap_Message_Rcvd :=
                 MOMA.Messages.MMaps.MMap (MOMA_Message_Temp);
            else
               raise Program_Error;
            end if;
         end;

         Ok := Get_Map (MMap_Message_Sent) = Get_Map (MMap_Message_Rcvd);
         Output ("Testing Map Message ", Ok);
      end if;

   end Test_MMap;

   -----------------------
   -- Text Message Test --
   -----------------------

   procedure Test_MText;

   procedure Test_MText
   is
      use MOMA.Messages.MTexts;
      use PolyORB.Any;

      MText_Message_Sent : MOMA.Messages.MTexts.MText;
      MText_Message_Rcvd : MOMA.Messages.MTexts.MText;
   begin
      --  Create new Text Message
      MText_Message_Sent := Create_Text_Message;
      Set_Text (MText_Message_Sent, To_MOMA_String ("Hi MOM !"));

      if Scenario in Full .. Stor then
         --  Send Text Message
         Send (MOMA_Producer, MText_Message_Sent);
      end if;

      if Scenario = Full or Scenario = Retr then
         --  Get Text Message
         declare
            MOMA_Message_Temp : MOMA.Messages.Message'Class
              := Receive (MOMA_Consumer);
         begin
            if MOMA_Message_Temp in MOMA.Messages.MTexts.MText then
               MText_Message_Rcvd :=
                 MOMA.Messages.MTexts.MText (MOMA_Message_Temp);
            else
               raise Program_Error;
            end if;
         end;

         --  Print results
         Ok := Get_Text (MText_Message_Sent)
           = Get_Text (MText_Message_Rcvd);

         Output ("Testing Text Message ", Ok);
      end if;

   end Test_MText;

   --------------------
   -- Main procedure --
   --------------------

begin

   --  Argument check
   if Argument_Count /= 3 then
      Put_Line ("usage : client <scenario> <kind> <IOR>");
      Put_Line (" where 'scenario' is in {full, stor, retr}");
      Put_Line ("  - full : full demo, send and receive messages");
      Put_Line ("  - stor : only send messages");
      Put_Line ("  - retr : only retrieve messages");
      New_Line;
      Put_Line (" where 'kind' is in {pool, naming}");
      Put_Line ("  - pool   : <IOR> is the IOR of a message pool");
      Put_Line ("  - naming : <IOR> is the IOR of a naming service");
      New_Line;
      Put_Line ("{stor, retr} scenarios are to test persistency");
      return;
   end if;

   --  Determine scenario to run
   if Ada.Command_Line.Argument (1) = "full" then
      Scenario := Full;
   elsif Ada.Command_Line.Argument (1) = "stor" then
      Scenario := Stor;
   elsif Ada.Command_Line.Argument (1) = "retr" then
      Scenario := Retr;
   end if;

   --  Get a reference on the message pool to use.
   if Ada.Command_Line.Argument (2) = "pool" then
      Pool_Ref := PolyORB.References.IOR.String_To_Object
        (To_PolyORB_String
         (Ada.Command_Line.Argument (3)));
   else
      Init (PolyORB.References.IOR.String_To_Object
            (To_PolyORB_String
             (Ada.Command_Line.Argument (3))));

      Pool_Ref := Locate ("Pool_1");
   end if;

   --  Initialize the connection factory
   --  (should be done by the administrator).
   MOMA.Connection_Factories.Queues.Create (MOMA_Factory, Pool_Ref);

   --  Create connection using Queue Connection Factory.
   MOMA_Connection := MOMA.Connections.Queues.Queue
      (MOMA.Connection_Factories.Queues.Create_Connection (MOMA_Factory));

   --  Initialize the destination
   --  (should be usually done by the administrator).
   --  NB : in this example the destination and the provider are references
   --       to the same thing (Pool_Ref). This will probably change later.
   MOMA_Destination := MOMA.Sessions.Queues.Create_Destination
      (To_MOMA_String ("queue1"), Pool_Ref);

   --  Create Session.
   MOMA_Session := Create_Session (MOMA_Connection, False, 1);

   --  Create Message Producer associated to the Session.
   MOMA_Producer := Create_Sender (MOMA_Session, MOMA_Destination);

   --  Create Message Consumer associated to the Session.
   MOMA_Consumer := Create_Receiver (MOMA_Session, MOMA_Destination);

   --  Initialization is completed.
   Output ("Initialization", True);

   --  Testing MAny messages.
   Test_MAny;

   --  Testing MByte messages.
   Test_MByte;

   --  Testing MMap messages.
   Test_MMap;

   --  Testing MText messages.
   Test_MText;

   --  XXX should destroy all structures here !
end Client;
