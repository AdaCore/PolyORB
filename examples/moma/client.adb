------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2007, Free Software Foundation, Inc.          --
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

--  Sample MOMA client

with Ada.Command_Line;
with Ada.Text_IO;

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);
--  XXX do not change Tasking model for now, otherwise there is a risk
--  of a race condition between producer and consumer ...

with MOMA.Connection_Factories;
with MOMA.Connections;
with MOMA.Sessions;
with MOMA.Destinations;

with MOMA.Message_Producers;
with MOMA.Message_Consumers;

with MOMA.Messages;
with MOMA.Messages.MAnys;
with MOMA.Messages.MBytes;
with MOMA.Messages.MMaps;
with MOMA.Messages.MTexts;

with MOMA.References;
with MOMA.Runtime;

with MOMA.Types;

with PolyORB.Utils.Report;

procedure Client is

   use Ada.Command_Line;
   use Ada.Text_IO;

   use MOMA.Connection_Factories;
   use MOMA.Sessions;
   use MOMA.Connections;
   use MOMA.Destinations;
   use MOMA.Message_Producers;
   use MOMA.Message_Consumers;
   use MOMA.Messages;
   use MOMA.Types;

   use PolyORB.Utils.Report;

   Naming_StringRef     : MOMA.Types.String;

   Pool_Ref           : MOMA.Types.Ref := MOMA.Types.Nil_Ref;
   Pool_StringRef     : MOMA.Types.String;

   Router_Ref         : MOMA.Types.Ref := MOMA.Types.Nil_Ref;
   Router_StringRef   : MOMA.Types.String;

   MOMA_Factory       : Connection_Factory;
   MOMA_Connection    : MOMA.Connections.Connection;
   MOMA_Session       : MOMA.Sessions.Session;
   MOMA_Dest_Router   : MOMA.Destinations.Destination;
   MOMA_Dest_Pool     : MOMA.Destinations.Destination;
   MOMA_Producer      : MOMA.Message_Producers.Message_Producer;
   MOMA_Consumer      : MOMA.Message_Consumers.Message_Consumer;
   MOMA_Consumer_Acc  : MOMA.Message_Consumers.Message_Consumer_Acc;

   Ok : Standard.Boolean;

   type Scenario_T is (Full, Stor, Retr, Sub, Unsub);
   Scenario : Scenario_T;

   type Kind_T is (Naming, Pool, Topic);
   Kind : Kind_T;

   ---------------
   -- Test_MAny --
   ---------------

   procedure Test_MAny;
   --  Test MAny message

   procedure Test_MAny
   is
      use MOMA.Messages.MAnys;

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

      if Scenario = Full
        or else Scenario = Retr
      then
         --  Get Any Message

         declare
            MOMA_Message_Temp : constant MOMA.Messages.Message'Class
              := Receive (MOMA_Consumer);
         begin
            if MOMA_Message_Temp in MOMA.Messages.MAnys.MAny then
               MAny_Message_Rcvd
                 := MOMA.Messages.MAnys.MAny (MOMA_Message_Temp);

            else
               raise Program_Error;
            end if;
         end;

         Ok := Get_Payload (MAny_Message_Sent)
           = Get_Payload (MAny_Message_Rcvd);
         Output ("Testing Any Message ", Ok);
      end if;

   end Test_MAny;

   ----------------
   -- Test_MByte --
   ----------------

   procedure Test_MByte;
   --  Test MByte message

   procedure Test_MByte
   is
      use MOMA.Messages.MBytes;

      MByte_Message_Sent : MOMA.Messages.MBytes.MByte;
      MByte_Message_Rcvd : MOMA.Messages.MBytes.MByte;

      procedure Send_Receive_MByte (Test_Name : String);

      procedure Send_Receive_MByte (Test_Name : String) is
      begin

         if Scenario in Full .. Stor then
            --  Send Byte message

            Send (MOMA_Producer, MByte_Message_Sent);
         end if;

         if Scenario = Full
           or else Scenario = Retr
         then
            --  Get Byte Message

            declare
               MOMA_Message_Temp : constant MOMA.Messages.Message'Class
                 := Receive (MOMA_Consumer);
            begin
               if MOMA_Message_Temp in MOMA.Messages.MBytes.MByte then
                  MByte_Message_Rcvd
                    := MOMA.Messages.MBytes.MByte (MOMA_Message_Temp);

               else
                  raise Program_Error;
               end if;
            end;

            Ok := Get_Payload (MByte_Message_Sent)
              = Get_Payload (MByte_Message_Rcvd);
            Output ("Testing " & Test_Name & " Message ", Ok);
         end if;

      end Send_Receive_MByte;

   begin

      --  Create new Byte Message

      MByte_Message_Sent := Create_Byte_Message;

      --  Byte/Boolean Test

      Set_Boolean (MByte_Message_Sent, MOMA.Types.Boolean'(True));
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

   ---------------
   -- Test_MMap --
   ---------------

   procedure Test_MMap;
   --  Test MMap message

   procedure Test_MMap
   is
      use MOMA.Messages.MMaps;

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

      if Scenario = Full
        or else Scenario = Retr
      then
         --  Get Map Message

         declare
            MOMA_Message_Temp : constant MOMA.Messages.Message'Class
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

   ----------------
   -- Test_MText --
   ----------------

   procedure Test_MText;
   --  Test MText message

   procedure Test_MText
   is
      use MOMA.Messages.MTexts;

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

      if Scenario = Full
        or else Scenario = Retr
      then
         --  Get Text Message

         declare
            MOMA_Message_Temp : constant MOMA.Messages.Message'Class
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

   ---------------
   -- Put_Usage --
   ---------------

   procedure Put_Usage;

   procedure Put_Usage is
   begin
      Put_Line ("usage : client <scenario> <kind> <IOR>");
      Put_Line (" where <scenario> is in {full, stor, retr}");
      Put_Line ("  - full : full demo, send and receive messages");
      Put_Line ("  - stor : only send messages");
      Put_Line ("  - retr : only retrieve messages");
      Put_Line (" where <kind> is in {pool, naming}");
      Put_Line ("  - pool   : <IOR> is the IOR of a message pool");
      Put_Line ("  - naming : <IOR> is the IOR of a naming service");
      New_Line;
      Put_Line ("or    : client stor topic <IOR>");
      Put_Line (" where <IOR> is the IOR of a router");
      New_Line;
      Put_Line ("or    : client <IOR>");
      Put_Line (" shortcut for client full pool <IOR>");
      New_Line;
      Put_Line ("or    : client <submode> <IOR1> <IOR2>");
      Put_Line (" where <submode> is in (sub, unsub)");
      Put_Line ("       <IOR1> is the IOR of the message pool to sub / unsub");
      Put_Line ("       <IOR2> is the IOR of a router");
      New_Line;
      Put_Line ("{stor, retr} scenarios are to test persistency");
   end Put_Usage;

   ---------------------
   -- Check_Arguments --
   ---------------------

   function Check_Arguments return Boolean;

   function Check_Arguments return Boolean is
      Arg1 : String renames Ada.Command_Line.Argument (1);

   begin
      if Argument_Count = 1 then
         Scenario := Full;
         Kind := Pool;
         Pool_StringRef := To_MOMA_String (Arg1);
         return True;
      end if;

      if Argument_Count /= 3 then
         return False;
      end if;

      declare
         Arg2 : String renames Ada.Command_Line.Argument (2);
         Arg3 : String renames Ada.Command_Line.Argument (3);
      begin
         if Arg1 = "full" then
            Scenario := Full;
         elsif Arg1 = "stor" then
            Scenario := Stor;
         elsif Arg1 = "retr" then
            Scenario := Retr;
         elsif Arg1 = "sub" then
            Scenario := Sub;
            Kind := Topic;
            return True;
         elsif Arg1 = "unsub" then
            Scenario := Unsub;
            Kind := Topic;
            return True;
         else
            return False;
         end if;

         if Arg2 = "pool" then
            Kind := Pool;
            Pool_StringRef := To_MOMA_String (Arg3);

         elsif Arg2 = "naming" then
            Kind := Naming;
            Naming_StringRef := To_MOMA_String (Arg3);

         elsif Arg2 = "topic" then
            Kind := Topic;
            Router_StringRef := To_MOMA_String (Arg3);
            Pool_StringRef := To_MOMA_String (Arg2);

            if Arg1 /= "stor" then
               return False;
            end if;
         else
            return False;
         end if;
      end;

      return True;
   end Check_Arguments;

   --  Start of processing for Client

begin

   --  Argument check

   if not (Check_Arguments) then
      Put_Usage;
      return;
   end if;

   --  Initialize MOMA

   MOMA.Runtime.Initialize;

   --  Get a reference on the message pool to use

   case Kind is
      when Pool =>
         MOMA.References.String_To_Reference
           (To_Standard_String (Pool_StringRef), Pool_Ref);

      when Naming =>
         MOMA.References.Initialize_Naming_Service
           (To_Standard_String (Naming_StringRef));
         Pool_Ref := MOMA.References.Locate ("Pool_1");
         Kind := Pool;

      when Topic =>
         MOMA.References.String_To_Reference
           (To_Standard_String (Router_StringRef), Router_Ref);

         if Scenario = Sub
           or else Scenario = Unsub
         then
            MOMA.References.String_To_Reference
              (To_Standard_String (Pool_StringRef), Pool_Ref);
         end if;
   end case;

   --  Initialize the connection factory
   --  (should be done by the administrator).

   MOMA.Connection_Factories.Create (MOMA_Factory, Pool_Ref);

   --  Create connection using Connection Factory

   MOMA_Connection
      := MOMA.Connections.Create_Connection (MOMA_Factory);

   --  Initialize the destination
   --  (should be usually done by the administrator).

   --  Note : in this example the destination and the provider are
   --  references to the same object (Pool_Ref). This will probably
   --  change later.

   if Pool_Ref /= Nil_Ref then
      MOMA_Dest_Pool := MOMA.Destinations.Create_Destination
         (To_MOMA_String ("queue1"),
          Pool_Ref,
          MOMA.Types.Pool);
   end if;

   if Router_Ref /= Nil_Ref then
      MOMA_Dest_Router := MOMA.Destinations.Create_Destination
         (To_MOMA_String ("Test"),
          Router_Ref,
          MOMA.Types.Topic);
   end if;

   --  Create Session

   MOMA_Session := Create_Session (MOMA_Connection, False, 1);

   --  Create Message Producer associated to the Session

   if Kind = Pool then
      MOMA_Producer := Create_Producer (MOMA_Session, MOMA_Dest_Pool);
   elsif Kind = Topic then
      MOMA_Producer := Create_Producer (MOMA_Session, MOMA_Dest_Router);
   end if;

   --  Create Message Consumer associated to the Session

   MOMA_Consumer_Acc := Create_Consumer (MOMA_Session, MOMA_Dest_Pool);
   MOMA_Consumer := MOMA_Consumer_Acc.all;

   --  Subscribe / Unsubscribe to the "Test" topic

   if Kind = Topic then
      if Scenario = Sub then
         MOMA.Sessions.Subscribe (MOMA_Dest_Router, MOMA_Dest_Pool);
      elsif Scenario = Unsub then
         MOMA.Sessions.Unsubscribe (MOMA_Dest_Router, MOMA_Dest_Pool);
      end if;
   end if;

   --  Initialization is completed

   Output ("Initialization", True);

   --  Testing MAny messages

   Test_MAny;

   --  Testing MByte messages

   Test_MByte;

   --  Testing MMap messages

   Test_MMap;

   --  Testing MText messages

   Test_MText;

   End_Report;
   --  XXX should destroy all structures here !
end Client;
