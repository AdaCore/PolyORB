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

--  Dummy MOMA client.

--  $Id$

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;

with PolyORB.Types;

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
with MOMA.Message_Consumers.Queues;

procedure Client is

   use MOMA.Connection_Factories.Queues;
   use MOMA.Sessions.Queues;
   use MOMA.Connections;
   use MOMA.Message_Producers.Queues;
   use MOMA.Message_Consumers.Queues;
   use PolyORB.Types;

   MOMA_Queue       : MOMA.Connections.Queues.Queue;
   MOMA_Session     : MOMA.Sessions.Queues.Queue;
   MOMA_Destination : MOMA.Destinations.Destination;
   MOMA_Producer    : MOMA.Message_Producers.Queues.Queue;
   MOMA_Consumer    : MOMA.Message_Consumers.Queues.Queue;

   Mesg_To_Send : PolyORB.Types.String := To_PolyORB_String ("Hi MOM !");
   Rcvd_Msg : PolyORB.Types.String;

begin
   --  Argument check
   if Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   --  Create Queue using Queue Connection Factory
   MOMA_Queue := Create (To_PolyORB_String (Ada.Command_Line.Argument (1)));

   --  Create Destination Queue associated to the connection
   MOMA_Destination := Create_Queue (MOMA_Queue,
                                     To_PolyORB_String ("queue1"));

   --  Create Session,
   MOMA_Session := Create_Session (False, 1);

   --  Create Message Producer associated to the Session
   MOMA_Producer := Create_Sender (MOMA_Session, MOMA_Destination);

   --  Create Message Consumer associated to the Session
   MOMA_Consumer := Create_Receiver (MOMA_Session, MOMA_Destination);

   --  Send message
   Send (MOMA_Producer, Mesg_To_Send);

   --  Get Message
   Rcvd_Msg := Receive (MOMA_Consumer);

   --  Print results
   Put_Line ("I sent     : " & To_String (Mesg_To_Send));
   Put_Line ("I received : " & To_String (Rcvd_Msg));

   --  End of File
   --  Put_Line ("waiting");
   --  delay 10.0;

   --  XXX should destroy all structures here !

end Client;
