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
with MOMA.Message_Consumers.Queues;
with MOMA.Messages;
with MOMA.Messages.MTexts;
with MOMA.Types;

procedure Client is

   use Ada.Command_Line;
   use Ada.Text_IO;

   use MOMA.Connection_Factories.Queues;
   use MOMA.Sessions.Queues;
   use MOMA.Connections;
   use MOMA.Message_Producers.Queues;
   use MOMA.Message_Consumers.Queues;
   use MOMA.Messages;
   use MOMA.Messages.MTexts;
   use MOMA.Types;

   MOMA_Queue        : MOMA.Connections.Queues.Queue;
   MOMA_Session      : MOMA.Sessions.Queues.Queue;
   MOMA_Destination  : MOMA.Destinations.Destination;
   MOMA_Producer     : MOMA.Message_Producers.Queues.Queue;
   MOMA_Consumer     : MOMA.Message_Consumers.Queues.Queue;
   MOMA_Message_Sent : MOMA.Messages.MTexts.MText;
   MOMA_Message_Rcvd : MOMA.Messages.MTexts.MText;

begin
   --  Argument check
   if Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   --  Create Queue using Queue Connection Factory
   MOMA_Queue := Create (To_MOMA_String (Ada.Command_Line.Argument (1)));

   --  Create Destination Queue associated to the connection
   MOMA_Destination := Create_Queue (MOMA_Queue,
                                     To_MOMA_String ("queue1"));

   --  Create Session,
   MOMA_Session := Create_Session (False, 1);

   --  Create Message Producer associated to the Session
   MOMA_Producer := Create_Sender (MOMA_Session, MOMA_Destination);

   --  Create Message Consumer associated to the Session
   MOMA_Consumer := Create_Receiver (MOMA_Session, MOMA_Destination);

   --  Create new Text Message
   MOMA_Message_Sent := Create_Text_Message;
   Set_Text (MOMA_Message_Sent, To_MOMA_String ("Hi MOM !"));

   --  Send message
   Send (MOMA_Producer, MOMA_Message_Sent);

   --  Get Message
   declare
      MOMA_Message_Temp : MOMA.Messages.Message'Class
        := Receive (MOMA_Consumer);
   begin
      if MOMA_Message_Temp in MOMA.Messages.MTexts.MText then
         MOMA_Message_Rcvd := MOMA.Messages.MTexts.MText (MOMA_Message_Temp);
      else
         raise Program_Error;
      end if;
   end;

   --  Print results
   Put_Line ("I sent     : " & Image (MOMA_Message_Sent));
   Put_Line ("I received : " & Image (MOMA_Message_Rcvd));

   --  End of File
   --  Put_Line ("waiting");
   --  delay 10.0;

   --  XXX should destroy all structures here !

end Client;
