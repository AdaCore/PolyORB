------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     C L I E N T _ C A L L _ B A C K                      --
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

--  Sample MOMA client with Message_Handler call_backs

--  $Id: //droopi/main/examples/moma/client_call_back.adb

with Ada.Command_Line;
with Ada.Text_IO;

with Client_Call_Back_Procedures;

with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);

with MOMA.Connection_Factories;
with MOMA.Connections;
with MOMA.Sessions;
with MOMA.Destinations;

with MOMA.Message_Producers;
with MOMA.Message_Consumers;
with MOMA.Message_Handlers;

with MOMA.Types;

with PolyORB.Initialization;
with PolyORB.References;
with PolyORB.Types;
with PolyORB.Utils.Report;

procedure Client_Call_Back is

   use Ada.Command_Line;
   use Ada.Text_IO;

   use Client_Call_Back_Procedures;

   use MOMA.Connection_Factories;
   use MOMA.Sessions;
   use MOMA.Connections;
   use MOMA.Destinations;
   use MOMA.Message_Producers;
   use MOMA.Message_Consumers;
   use MOMA.Message_Handlers;
   use MOMA.Types;

   use PolyORB.References;
   use PolyORB.Types;
   use PolyORB.Utils.Report;

   Pool_Ref           : PolyORB.References.Ref := PolyORB.References.Nil_Ref;
   MOMA_Factory       : Connection_Factory;
   MOMA_Connection    : MOMA.Connections.Connection;
   MOMA_Session       : MOMA.Sessions.Session;
   MOMA_Dest_Pool     : MOMA.Destinations.Destination;
   MOMA_Producer      : MOMA.Message_Producers.Message_Producer;
   MOMA_Consumer      : MOMA.Message_Consumers.Message_Consumer;
   MOMA_Consumer_Acc  : MOMA.Message_Consumers.Message_Consumer_Acc;
   MOMA_Handler       : MOMA.Message_Handlers.Message_Handler;
   MOMA_Handler_Acc   : MOMA.Message_Handlers.Message_Handler_Acc;
   Message_Id         : MOMA.Types.Byte;

   ----------
   -- Wait --
   ----------

   procedure Wait;

   procedure Wait is
      Data : Byte_Test_Note;
   begin
      Get_Call_Back_Data (MOMA_Handler_Acc, Data);
      while not Data.Proceed loop
         delay 0.5;
         Get_Call_Back_Data (MOMA_Handler_Acc, Data);
      end loop;
      Data.Proceed := False;
      Set_Call_Back_Data (MOMA_Handler_Acc, Data);
   end Wait;

   --------------------
   -- Main Procedure --
   --------------------

begin

   --  Argument check

   if Argument_Count /= 1 then
      Put_Line ("usage : client <IOR>");
      return;
   end if;

   Put_Line ("Initialize");

   --  Initialize World

   PolyORB.Initialization.Initialize_World;

   --  Get a reference on the message pool to use.

   PolyORB.References.String_To_Object
     (Ada.Command_Line.Argument (1), Pool_Ref);

   --  Initialize the connection factory
   --  (should be done by the administrator).

   MOMA.Connection_Factories.Create (MOMA_Factory, Pool_Ref);

   --  Create connection using Connection Factory.
   MOMA_Connection
     := MOMA.Connections.Create_Connection (MOMA_Factory);

   --  Initialize the destination
   --  (should be usually done by the administrator).

   --  Note : in this example the destination and the provider are
   --  references to the same object (Pool_Ref). This will probably
   --  change later.

   MOMA_Dest_Pool := MOMA.Destinations.Create_Destination
         (To_MOMA_String ("queue1"),
          Pool_Ref);

   --  Create Session

   MOMA_Session := Create_Session (MOMA_Connection, False, 1);

   --  Create Message Producer associated to the Session

   MOMA_Producer := Create_Producer (MOMA_Session, MOMA_Dest_Pool);

   --  Create Message Consumer associated to the Session

   MOMA_Consumer_Acc := Create_Consumer (MOMA_Session, MOMA_Dest_Pool);
   MOMA_Consumer := MOMA_Consumer_Acc.all;

   MOMA_Handler_Acc := Create_Handler (MOMA_Session, MOMA_Consumer_Acc);

   MOMA_Handler := MOMA_Handler_Acc.all;

   --  Initialization is completed
   Output ("Initialization", True);

   --  Test #1

   Set_Byte_Test_Note (MOMA_Handler_Acc,
                       Byte_Value => MOMA.Types.Byte (1),
                       Proceed => False);

   Set_Handler (MOMA_Handler_Acc, Handle_Then_Notify'Access);
   Set_Notifier (MOMA_Handler_Acc, Notify_And_Receive'Access);
   Set_Behavior (MOMA_Handler_Acc, Handle);
   Output ("Set behavior and procedures", True);

   Put_Line ("Send messages");
   Send_MByte (MOMA_Producer, 1);
   --  Message 1 is handled.
   --  Behavior is set to Notify by current Handle procedure.
   Wait;
   Output ("Test #1", True);

   --  Test #2

   Set_Behavior (MOMA_Handler_Acc, Notify);

   Set_Byte_Test_Note (MOMA_Handler_Acc,
                       Byte_Value => MOMA.Types.Byte (2),
                       Proceed => False);
   Send_MByte (MOMA_Producer, 2);
   --  Message 2 is notified and received.

   Wait;
   Output ("Test #2", True);

   --  Test #3

   Set_Notifier (MOMA_Handler_Acc,
                 MOMA.Message_Handlers.Template_Notifier'Access);

   Send_MByte (MOMA_Producer, 3);
   --  Message 3 is notified and not received

   Output ("Test #3", True);

   --  Test #4

   Set_Notifier (MOMA_Handler_Acc, Notify_Then_Handle'Access);

   Send_MByte (MOMA_Producer, 4);
   --  Message 4 is notified and not received.  Behavior is set to
   --  Handle by current Notify procedure.

   Wait;
   Output ("Test #4", True);

   --  Test #5

   Set_Byte_Test_Note (MOMA_Handler_Acc,
                       Byte_Value => MOMA.Types.Byte (5),
                       Proceed => False);

   Send_MByte (MOMA_Producer, 5);
   --  Message 5 is handled.
   --  Behavior is set to Notify by current Handle procedure.

   Wait;
   Output ("Test #5", True);

   --  Test #6

   Set_Behavior (MOMA_Handler_Acc, None);

   Send_MByte (MOMA_Producer, 6);
   --  No call_back actions are defined for Message 6

   Output ("Test #6", True);

   --  Test #7

   Message_Id := Receive_MByte (MOMA_Consumer);
   Output ("Receive message " & MOMA.Types.Byte'Image (Message_Id),
      Message_Id = MOMA.Types.Byte (3));

   Message_Id := Receive_MByte (MOMA_Consumer);
   Output ("Receive message " & MOMA.Types.Byte'Image (Message_Id),
      Message_Id = MOMA.Types.Byte (4));

   Message_Id := Receive_MByte (MOMA_Consumer);
   Output ("Receive message " & MOMA.Types.Byte'Image (Message_Id),
      Message_Id = MOMA.Types.Byte (6));

   --  XXX should destroy all structures here !

   Output ("Test #7", True);
   Output ("End of tests", True);

end Client_Call_Back;
