------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 M O M A . S E S S I O N S . Q U E U E S                  --
--                                                                          --
--                                 S p e c                                  --
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

--  Derivation of the Session type for Queues. See MOMA.Sessions for more
--  details.

--  $Id$

with MOMA.Connections.Queues;
with MOMA.Destinations;
with MOMA.Message_Consumers.Queues;
with MOMA.Message_Producers.Queues;
with MOMA.Provider.Message_Handler;
with MOMA.Types;

with PolyORB.References;

package MOMA.Sessions.Queues is

   type Session_Queue is new Session with null record;

   function Create_Destination (Name   : MOMA.Types.String;
                                Remote : PolyORB.References.Ref)
                                return MOMA.Destinations.Destination;
   --  Create a destination whose target is the Remote reference (currently a
   --  message pool).

   function Create_Session (Connection       : MOMA.Connections.Queues.Queue;
                            Transacted       : Boolean;
                            Acknowledge_Mode : MOMA.Types.Acknowledge_Type)
                            return MOMA.Sessions.Queues.Session_Queue;
   --  Create a session from a Connection.

   function Create_Receiver (Self : Session_Queue;
                             Dest : MOMA.Destinations.Destination)
                             return MOMA.Message_Consumers.Queues.Queue_Acc;

   function Create_Receiver (Queue            : MOMA.Destinations.Destination;
                             Message_Selector : MOMA.Types.String)
                             return MOMA.Message_Consumers.Queues.Queue;

   function Create_Handler
     (Self  : Session_Queue;
      Message_Queue : MOMA.Message_Consumers.Queues.Queue_Acc)
      return MOMA.Provider.Message_Handler.Object_Acc;
   --  Create a Message Handler associated to the specified Message queue.
   --  Must set the Handler and Notifier procedures afterwards.

   function Create_Sender (Self : Session_Queue;
                           Dest : MOMA.Destinations.Destination)
                           return MOMA.Message_Producers.Queues.Queue;
   --  Create a 'sender', a message producer, its destination is a MOM object.

   function Create_Sender (ORB_Object : MOMA.Types.String;
                           Mesg_Pool  : MOMA.Types.String)
                           return MOMA.Message_Producers.Queues.Queue;
   --  Create a 'sender', a message producer, its destination is an ORB object.

   function Create_Temporary return MOMA.Destinations.Destination;

end MOMA.Sessions.Queues;
