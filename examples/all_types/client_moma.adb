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
with MOMA.Sessions.Queues;
with MOMA.Destinations;

with MOMA.Message_Producers.Queues;

with MOMA.Messages;
with MOMA.Messages.MExecutes;
with MOMA.Types;

with PolyORB.Any;

with Report;

procedure Client_MOMA is
pragma Warnings (Off);

   use Ada.Command_Line;
   use Ada.Text_IO;

   use MOMA.Connection_Factories.Queues;
   use MOMA.Sessions.Queues;
   use MOMA.Connections;
   use MOMA.Message_Producers.Queues;
   use MOMA.Messages;
   use MOMA.Messages.MExecutes;
   use MOMA.Types;

   use Report;

   MOMA_Queue         : MOMA.Connections.Queues.Queue;
   MOMA_Session       : MOMA.Sessions.Queues.Queue;
   MOMA_Destination   : MOMA.Destinations.Destination;
   MOMA_Producer      : MOMA.Message_Producers.Queues.Queue;

   Ok : Boolean;

   --------------------------
   -- Execute Message Test --
   --------------------------

   procedure Test_MExecute;

   procedure Test_MExecute
   is
      use PolyORB.Any;

      MExecute_Message_Sent : MOMA.Messages.MExecutes.MExecute;
      MExecute_Message_Rcvd : MOMA.Messages.MExecutes.MExecute;

      Method_Name   : Map_Element;
      Return_1      : Map_Element;
      Arg_1         : Map_Element;
      Parameter_Map : Map;

   begin
      --  Create new Text Message
      MExecute_Message_Sent := Create_Execute_Message;
      Method_Name := (Name  => To_MOMA_String ("method"),
                      Value => To_Any (To_MOMA_String ("echoString")));

      Return_1 := (Name  => To_MOMA_String ("return_1"),
                   Value => To_Any (To_MOMA_String ("")));

      Arg_1 := (Name  => To_MOMA_String ("arg_1"),
                Value => To_Any (To_MOMA_String ("Hi Mom !")));

      Append (Parameter_Map, Method_Name);
      Append (Parameter_Map, Return_1);
      Append (Parameter_Map, Arg_1);

      Set_Parameter (MExecute_Message_Sent, Parameter_Map);

      Send (MOMA_Producer, MExecute_Message_Sent);

   end Test_MExecute;

   --------------------
   -- Main procedure --
   --------------------

begin
   --  Argument check.
   if Argument_Count < 1 then
      Put_Line ("usage : client_moma <IOR_string_from_server>");
      return;
   end if;

   --  Create Queue using Queue Connection Factory.
   MOMA_Queue := Create (To_MOMA_String (Ada.Command_Line.Argument (1)));

   --  Create Destination Queue associated to the connection.
   MOMA_Destination := Create_Queue (MOMA_Queue,
                                     To_MOMA_String ("orb_object_1"));

   --  Create Session.
   MOMA_Session := Create_Session (False, 1);

   --  Create Message Producer associated to the Session.
   MOMA_Producer := Create_Sender (To_MOMA_String (Argument (1)),
                                   To_MOMA_String (""));

   Output ("Initilisation", True);

   --  Testing MExecute.

   Test_MExecute;


pragma Warnings (On);
end Client_MOMA;
