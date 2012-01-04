------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     C L I E N T _ C A L L _ B A C K                      --
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

--  Sample MOMA client with Message_Handler call_backs

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

with MOMA.References;
with MOMA.Runtime;

with MOMA.Types;

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

   use PolyORB.Utils.Report;

   Pool_Ref           : MOMA.Types.Ref := MOMA.Types.Nil_Ref;
   MOMA_Factory       : Connection_Factory;
   MOMA_Connection    : MOMA.Connections.Connection;
   MOMA_Session       : MOMA.Sessions.Session;
   MOMA_Dest_Pool     : MOMA.Destinations.Destination;
   MOMA_Producer      : MOMA.Message_Producers.Message_Producer;
   MOMA_Consumer      : MOMA.Message_Consumers.Message_Consumer;
   MOMA_Consumer_Acc  : MOMA.Message_Consumers.Message_Consumer_Acc;
   MOMA_Handler_Acc   : MOMA.Message_Handlers.Message_Handler_Acc;
   MOMA_Handler       : MOMA.Message_Handlers.Message_Handler;
   --  pragma Unreferenced (MOMA_Handler);
   pragma Warnings (Off, MOMA_Handler); --  WAG:5.02 DB08-008
   --  Assigned but never read

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
      Put_Line ("usage : client_call_back <IOR>");
      return;
   end if;

   --  Initialize MOMA

   MOMA.Runtime.Initialize;

   --  Get a reference on the message pool to use.

   MOMA.References.String_To_Reference
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
   Output ("Receive message" & MOMA.Types.Byte'Image (Message_Id),
      Message_Id = MOMA.Types.Byte (3));

   Message_Id := Receive_MByte (MOMA_Consumer);
   Output ("Receive message" & MOMA.Types.Byte'Image (Message_Id),
      Message_Id = MOMA.Types.Byte (4));

   Message_Id := Receive_MByte (MOMA_Consumer);
   Output ("Receive message" & MOMA.Types.Byte'Image (Message_Id),
      Message_Id = MOMA.Types.Byte (6));

   --  XXX should destroy all structures here !

   Output ("Test #7", True);
   End_Report;

end Client_Call_Back;
