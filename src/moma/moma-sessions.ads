------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        M O M A . S E S S I O N S                         --
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

--  A Session defines an execution context in which Message_Producers and
--  Message_Consumers live. We use the capabilities of PolyORB's POA to
--  associate a thread to each session.

--  XXX this package requires first to complete the implementation of POA
--  policies. Its definition and the completion of its API is left 'as is'.

--  $Id$

with MOMA.Destinations;
with MOMA.Message_Consumers;
with MOMA.Message_Producers;
with MOMA.Message_Handlers;
with MOMA.Types;

with PolyORB.References;

package MOMA.Sessions is

   type Session is record
      Transacted : Boolean;
      Acknowledge_Mode : MOMA.Types.Acknowledge_Type;
   end record;

   procedure Close;

   procedure Commit;

   function Get_Transacted return Boolean;

   procedure Recover;

   procedure Rollback;

   function Create_Receiver (Self : Session;
                             Dest : MOMA.Destinations.Destination)
      return MOMA.Message_Consumers.Message_Consumer_Acc;

   function Create_Receiver (Dest             : MOMA.Destinations.Destination;
                             Message_Selector : MOMA.Types.String)
      return MOMA.Message_Consumers.Message_Consumer;

   --  XXX Add functions, or modify existing ones, to implement durable
   --  subscription to a topic.

   function Create_Handler
     (Self           : Session;
      Message_Cons   : MOMA.Message_Consumers.Message_Consumer_Acc)
      return MOMA.Message_Handlers.Message_Handler_Acc;
   --  Create a Message Handler associated to the specified Message consumer.
   --  Must set the Handler and Notifier procedures afterwards.

   function Create_Sender (Self : Session;
                           Dest : MOMA.Destinations.Destination)
                           return MOMA.Message_Producers.Message_Producer;
   --  Create a 'sender', a message producer, its destination is a MOM object.

   function Create_Sender (ORB_Object : MOMA.Types.String;
                           Mesg_Pool  : MOMA.Types.String)
                           return MOMA.Message_Producers.Message_Producer;
   --  Create a 'sender', a message producer, its destination is an ORB object.

   function Create_Temporary return MOMA.Destinations.Destination;
   --  XXX Not implemented.

   procedure Subscribe (Topic : MOMA.Destinations.Destination;
                        Pool  : MOMA.Destinations.Destination);
   --  Subscribe a Pool to a Topic. See MOMA.Destinations for more details.

   procedure Unsubscribe (Topic : MOMA.Destinations.Destination;
                          Pool  : MOMA.Destinations.Destination);
   --  Unsubscribe a Pool from a Topic. See MOMA.Destinations for more details.

   function Create_Destination (Name   : MOMA.Types.String;
                                Remote : PolyORB.References.Ref)
      return MOMA.Destinations.Destination;
   --  Create a destination whose target is the Remote reference (currently a
   --  message pool).

   --  XXX These three procedures would rather be elsewhere.

end MOMA.Sessions;
