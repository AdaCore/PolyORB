------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      C L I E N T _ C A L L _ B A C K                     --
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

--  Testing MOMA client with Message_Handler call_backs.

--  $Id: //droopi/main/examples/moma/client_call_back

with Ada.Command_Line;
with Ada.Text_IO;

with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);
--  XXX this package should be renamed to PolyORB.Setup.Thread_Pool_Node ...

with MOMA.Connection_Factories;
with MOMA.Connections;
with MOMA.Sessions;
with MOMA.Destinations;

with MOMA.Message_Producers;
with MOMA.Message_Consumers;
with MOMA.Message_Handlers;

with MOMA.Messages;
with MOMA.Messages.MBytes;

with MOMA.Types;

with PolyORB.Initialization;
with PolyORB.References;
with PolyORB.References.IOR;

with Report;

procedure Client_Call_Back is

   use Ada.Command_Line;
   use Ada.Text_IO;

   use MOMA.Connection_Factories;
   use MOMA.Sessions;
   use MOMA.Connections;
   use MOMA.Destinations;
   use MOMA.Message_Producers;
   use MOMA.Message_Consumers;
   use MOMA.Message_Handlers;
   use MOMA.Messages;
   use MOMA.Messages.MBytes;
   use MOMA.Types;

   use PolyORB.References;

   use Report;

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
   Ok : Boolean;

   Message_Id         : MOMA.Types.Byte;

   -----------------------
   -- Byte Message Test --
   -----------------------

   procedure Send_MByte (Id : MOMA.Types.Byte);

   procedure Send_MByte (Id : MOMA.Types.Byte)
   is

      MByte_Message_Sent : MOMA.Messages.MBytes.MByte := Create_Byte_Message;

   begin
      Set_Byte (MByte_Message_Sent, Id);
      Send (MOMA_Producer, MByte_Message_Sent);
   end Send_MByte;


   function Get_Byte_Value (Message : MOMA.Messages.Message'Class)
      return MOMA.Types.Byte;

   function Get_Byte_Value (Message : MOMA.Messages.Message'Class)
                           return MOMA.Types.Byte
   is
      MByte_Message_Rcvd : MOMA.Messages.MBytes.MByte;

   begin
      if Message in MOMA.Messages.MBytes.MByte then
         MByte_Message_Rcvd :=
           MOMA.Messages.MBytes.MByte (Message);
      else
         raise Program_Error;
      end if;
      return Get_Byte (MByte_Message_Rcvd);
   end Get_Byte_Value;


   function Receive_MByte return MOMA.Types.Byte;

   function Receive_MByte return MOMA.Types.Byte is
      MOMA_Message_Temp : MOMA.Messages.Message'Class
         := Receive (MOMA_Consumer);
   begin
      return Get_Byte_Value (MOMA_Message_Temp);
   end Receive_MByte;

   --------------------------
   -- Call_Back Procedures --
   --------------------------

   procedure P_Handler1 (
      MOMA_Handler : access Message_Handler;
      Message : MOMA.Messages.Message'Class);
   pragma Warnings (Off);
   pragma Unreferenced (P_Handler1);
   pragma Warnings (On);

   procedure P_Handler1 (
      MOMA_Handler : access Message_Handler;
      Message : MOMA.Messages.Message'Class)
   is
      pragma Warnings (Off);
      pragma Unreferenced (MOMA_Handler);
      pragma Warnings (On);

   begin
      Put_Line ("Handling Message " &
         MOMA.Types.Byte'Image (Get_Byte_Value (Message)));
   end P_Handler1;

   procedure P_Notifier1 (MOMA_Handler : access Message_Handler);
   pragma Warnings (Off);
   pragma Unreferenced (P_Notifier1);
   pragma Warnings (On);

   procedure P_Notifier1 (MOMA_Handler : access Message_Handler)
   is
      pragma Warnings (Off);
      pragma Unreferenced (MOMA_Handler);
      pragma Warnings (On);

   begin
      Put_Line ("Notifying Message");
   end P_Notifier1;

   Handler1 : MOMA.Message_Handlers.Handler;

   Notifier1 : MOMA.Message_Handlers.Notifier;

   ---------------
   -- Put_Usage --
   ---------------

   procedure Put_Usage;

   procedure Put_Usage
   is
   begin
      Put_Line ("usage : client <IOR>");
   end Put_Usage;

   --------------------
   -- Main Procedure --
   --------------------

begin

   --  Argument check
   if Argument_Count /= 1 then
      Put_Usage;
      return;
   end if;

   Put_Line ("Initialize");

   --  Initialize World
   PolyORB.Initialization.Initialize_World;

   --  Get a reference on the message pool to use.
   Pool_Ref := PolyORB.References.IOR.String_To_Object (
      MOMA.Types.To_MOMA_String (Ada.Command_Line.Argument (1)));

   --  Initialize the connection factory
   --  (should be done by the administrator).
   MOMA.Connection_Factories.Create (MOMA_Factory, Pool_Ref);

   --  Create connection using Connection Factory.
   MOMA_Connection :=
      MOMA.Connections.Create_Connection (MOMA_Factory);

   --  Initialize the destination
   --  (should be usually done by the administrator).
   --  NB : in this example the destination and the provider are references
   --       to the same thing (Pool_Ref). This will probably change later.
   MOMA_Dest_Pool := MOMA.Destinations.Create_Destination
         (To_MOMA_String ("queue1"),
          Pool_Ref);

   --  Create Session.
   MOMA_Session := Create_Session (MOMA_Connection, False, 1);

   --  Create Message Producer associated to the Session.
   MOMA_Producer := Create_Producer (MOMA_Session, MOMA_Dest_Pool);

   --  Create Message Consumer associated to the Session.
   MOMA_Consumer_Acc := Create_Consumer (MOMA_Session, MOMA_Dest_Pool);
   MOMA_Consumer := MOMA_Consumer_Acc.all;

   MOMA_Handler_Acc := Create_Handler (
      MOMA_Session, MOMA_Consumer_Acc);

   MOMA_Handler := MOMA_Handler_Acc.all;

   --  Initialization is completed.
   Output ("Initialization", True);

   Handler1 := MOMA.Message_Handlers.Template_Handler'Access;

   Notifier1 := MOMA.Message_Handlers.Template_Notifier'Access;

   Set_Handler (MOMA_Handler_Acc, Handler1);
   Set_Notifier (MOMA_Handler_Acc, Notifier1);
   Set_Behavior (MOMA_Handler_Acc, Notify);

   Send_MByte (1);

   Set_Behavior (MOMA_Handler_Acc, Handle);

   Send_MByte (2);

   Message_Id := Receive_MByte;
   Message_Id := Receive_MByte;
   Ok := True;

   --  Output ("Testing " & Test_Name & " Message ", Ok);

   --  XXX should destroy all structures here !
end Client_Call_Back;
