------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 M O M A . S E S S I O N S . Q U E U E S                  --
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

--  $Id$

with MOMA.Connections.Queues;
with MOMA.Destinations;
with MOMA.Message_Consumers;
with MOMA.Message_Producers;
with MOMA.Provider.Message_Consumer;
with MOMA.Provider.Message_Producer;
with MOMA.Types;
with PolyORB.MOMA_P.Tools;
with PolyORB.References;

package body MOMA.Sessions.Queues is

   use PolyORB.MOMA_P.Tools;
   use MOMA.Message_Producers;
   use MOMA.Message_Consumers;
   use MOMA.Destinations;
   use MOMA.Connections.Queues;

   ------------------
   -- Create_Queue --
   ------------------

   function Create_Queue
     (Connection : MOMA.Connections.Queues.Queue;
      Queue_Name : MOMA.Types.String)
      return MOMA.Destinations.Destination
   is
      Dest_Queue : MOMA.Destinations.Destination;
   begin
      Set_Name (Dest_Queue, Queue_Name);
      Set_Ref  (Dest_Queue, Get_Ref (Connection));
      return Dest_Queue;
   end Create_Queue;

   --------------------
   -- Create_Session --
   --------------------

   function Create_Session
     (Transacted : Boolean;
      Acknowledge_Mode : MOMA.Types.Acknowledge_Type)
      return MOMA.Sessions.Queues.Queue
   is
      Queue : MOMA.Sessions.Queues.Queue;
   begin
      Queue.Transacted := Transacted;
      Queue.Acknowledge_Mode := Acknowledge_Mode;
      return Queue;
   end Create_Session;

   ---------------------
   -- Create_Receiver --
   ---------------------

   function Create_Receiver
     (Self  : Queue;
      Dest  : MOMA.Destinations.Destination)
      return MOMA.Message_Consumers.Queues.Queue
   is
      MOMA_Obj : constant MOMA.Provider.Message_Consumer.Object_Acc
        := new MOMA.Provider.Message_Consumer.Object;

      MOMA_Ref : PolyORB.References.Ref;

      Queue : MOMA.Message_Consumers.Queues.Queue;

   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      --  XXX self is to be used to 'place' the receiver
      --  using session position in the POA

      MOMA_Obj.Remote_Ref := Get_Ref (Dest);
      Initiate_Servant (MOMA_Obj,
                        MOMA.Provider.Message_Producer.If_Desc,
                        MOMA_Ref);

      Set_Destination (Queue, Dest);
      Set_Ref (Message_Consumer (Queue), MOMA_Ref);
      --  XXX Is it really useful to have the Ref to the remote queue in the
      --  Message_Producer itself ? By construction, this ref is encapsulated
      --  in the MOMA.Provider.Message_Producer.Object ....
      return Queue;
   end Create_Receiver;

   ---------------------
   -- Create_Receiver --
   ---------------------

   function Create_Receiver
     (Queue : MOMA.Destinations.Destination;
      Message_Selector : MOMA.Types.String)
      return MOMA.Message_Consumers.Queues.Queue
   is
   begin
      pragma Warnings (Off);
      return Create_Receiver (Queue, Message_Selector);
      pragma Warnings (On);
   end Create_Receiver;

   -------------------
   -- Create_Sender --
   -------------------

   function Create_Sender
     (Self : Queue;
      Dest : MOMA.Destinations.Destination)
      return MOMA.Message_Producers.Queues.Queue
   is
      MOMA_Obj : constant MOMA.Provider.Message_Producer.Object_Acc
        := new MOMA.Provider.Message_Producer.Object;

      MOMA_Ref : PolyORB.References.Ref;

      Queue : MOMA.Message_Producers.Queues.Queue;

   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      --  XXX self is to be used to 'place' the receiver
      --  using session position in the POA

      MOMA_Obj.Remote_Ref := Get_Ref (Dest);
      Initiate_Servant (MOMA_Obj,
                        MOMA.Provider.Message_Producer.If_Desc,
                        MOMA_Ref);

      Set_Destination (Queue, Dest);
      Set_Ref (Message_Producer (Queue), MOMA_Ref);
      --  XXX Is it really useful to have the Ref to the remote queue in the
      --  Message_Producer itself ? By construction, this ref is encapsulated
      --  in the MOMA.Provider.Message_Producer.Object ....
      return Queue;
   end Create_Sender;

   ----------------------
   -- Create_Temporary --
   ----------------------

   function Create_Temporary
     return MOMA.Destinations.Destination is
   begin
      pragma Warnings (Off);
      return Create_Temporary;
      pragma Warnings (On);
   end Create_Temporary;

end MOMA.Sessions.Queues;

