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

with MOMA.Destinations;
with MOMA.Message_Consumers;
with MOMA.Message_Producers;
with MOMA.Message_Producers.Queues;
with MOMA.Provider.Message_Consumer;
with MOMA.Provider.Message_Producer;
with MOMA.Types;

with PolyORB.Annotations;
with PolyORB.Call_Back;
with PolyORB.Minimal_Servant.Tools;
with PolyORB.References;
with PolyORB.References.IOR;
with PolyORB.Types;

package body MOMA.Sessions.Queues is

   use MOMA.Message_Producers;
   use MOMA.Message_Consumers;
   use MOMA.Destinations;

   use PolyORB.Minimal_Servant.Tools;

   ------------------------
   -- Create_Destination --
   ------------------------

   function Create_Destination (Name   : MOMA.Types.String;
                                Remote : PolyORB.References.Ref)
                                return MOMA.Destinations.Destination
   is
   begin
      return Create (Name, Remote);
   end Create_Destination;

   --------------------
   -- Create_Session --
   --------------------

   function Create_Session (Connection       : MOMA.Connections.Queues.Queue;
                            Transacted       : Boolean;
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
                        MOMA.Types.MOMA_Type_Id,
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
      raise PolyORB.Not_Implemented;
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
      use PolyORB.References;
      use MOMA.Types;

      MOMA_Obj : constant MOMA.Provider.Message_Producer.Object_Acc
        := new MOMA.Provider.Message_Producer.Object;

      MOMA_Ref : PolyORB.References.Ref;
      Queue : MOMA.Message_Producers.Queues.Queue;
      Type_Id_S : MOMA.Types.String
        := To_MOMA_String (Type_Id_Of (Get_Ref (Dest)));
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      --  XXX self is to be used to 'place' the receiver
      --  using session position in the POA

      MOMA_Obj.Remote_Ref := Get_Ref (Dest);
      Initiate_Servant (MOMA_Obj,
                        MOMA.Provider.Message_Producer.If_Desc,
                        MOMA.Types.MOMA_Type_Id,
                        MOMA_Ref);

      Set_Destination (Queue, Dest);
      Set_Ref (Message_Producer (Queue), MOMA_Ref);
      Set_Type_Id_Of (Message_Producer (Queue), Type_Id_S);
      --  XXX Is it really useful to have the Ref to the remote queue in the
      --  Message_Producer itself ? By construction, this ref is encapsulated
      --  in the MOMA.Provider.Message_Producer.Object ....
      return Queue;
   end Create_Sender;

   function Create_Sender (ORB_Object : MOMA.Types.String;
                           Mesg_Pool  : MOMA.Types.String)
                           return MOMA.Message_Producers.Queues.Queue
   is

      use MOMA.Types;
      use MOMA.Message_Producers.Queues;

      use PolyORB.Annotations;
      use PolyORB.Call_Back;
      use PolyORB.References;
      use PolyORB.References.IOR;
      use PolyORB.Types;

      Queue : MOMA.Message_Producers.Queues.Queue;

      ORB_Object_IOR      : constant IOR_Type := String_To_Object (ORB_Object);
      Dest_Ref_Object_IOR : constant IOR_Type := String_To_Object (Mesg_Pool);

      Type_Id_S : MOMA.Types.String
        := To_MOMA_String (Type_Id_Of (ORB_Object_IOR));

   begin
      if Type_Id_S = MOMA_Type_Id then
         raise  Program_Error;
      end if;

      Set_Ref (Message_Producer (Queue), ORB_Object_IOR);
      Set_Type_Id_Of (Message_Producer (Queue), Type_Id_S);
      Queue.CBH := new PolyORB.Call_Back.Call_Back_Handler;
      --  XXX should free this memory sometime, somewhere ...

      Attach_Handler_To_CB
        (Call_Back_Handler (Queue.CBH.all),
         MOMA.Message_Producers.Queues.Response_Handler'Access);

      Set_Note
         (Notepad_Of (Queue.CBH).all,
          CBH_Note'(Note with Dest => Dest_Ref_Object_IOR));

      return Queue;
   end Create_Sender;

   ----------------------
   -- Create_Temporary --
   ----------------------

   function Create_Temporary
     return MOMA.Destinations.Destination is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Create_Temporary;
      pragma Warnings (On);
   end Create_Temporary;

end MOMA.Sessions.Queues;

