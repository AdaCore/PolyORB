------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        M O M A . P R O V I D E R . M E S S A G E _ H A N D L E R         --
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

--  Actual implementation of the Message_Handler object.
--  It is derived from PolyORB's Minimal_Servant.
--  The call-back purpose of a Message Handler is to receive a Request from
--  the actual message queue servant when a message is received : this Request
--  can either be Handle (then the message can not be recovered by a call to
--  the Message_Consumer's Receive and has to be treated by the Handle
--  procedure), or Notify (then the message stays in the pool).

--  $Id$

with PolyORB.Minimal_Servant;
with PolyORB.Requests;
with PolyORB.Obj_Adapters.Simple;
with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.References;

with MOMA.Messages;
with MOMA.Message_Consumers.Queues;
with MOMA.Types;
use MOMA.Message_Consumers.Queues;

package MOMA.Provider.Message_Handler is

   type Object is new PolyORB.Minimal_Servant.Servant with private;

   type Object_Acc is access Object;

   type Handler is access procedure (Message_Queue : Queue;
                                     Message : MOMA.Messages.Message'Class);
   --  The procedure to be called when a message is received.

   type Notifier is access procedure (Message_Queue : Queue);

   function Initialize (Message_Queue : Queue_Acc;
                        New_Notifier_Procedure : in Notifier := null;
                        New_Handler_Procedure : in Handler := null
   )
     return PolyORB.References.Ref;
   --  Initialize the Message_Handler and return its Reference.
   --  If both Notifier and Handler procedure are given a not null value,
   --  the Behavior is set to Notify and the Handler procedure will be
   --  treated as null.

   procedure Invoke (Self : access Object;
                     Req  : in     PolyORB.Requests.Request_Access);
   --  Message_Handler servant skeleton.

   procedure Set_Handler (Self : access Object;
                          New_Handler_Procedure : in Handler);
   --  Associate a Handler procedure to the Message Handler.
   --  Replace the current Handler procedure and set the Notifier procedure
   --  to null (even if the Handler procedure is null).
   --  A request is sent to the actual queue to change its behaviour.
   --  The actual queue will request the Message Handler to Handle the message
   --  rather than just Notify it.

   procedure Set_Notifier (Self : access Object;
                           New_Notifier_Procedure : in Notifier);
   --  Symmetric of Set_Handler
   --  Messages will not be handled any more, but just notified.


   procedure Set_Queue (Self : access Object;
                        New_Queue : Queue_Acc);
   --  Set the message queue which handles the messages.

   function If_Desc
     return PolyORB.Obj_Adapters.Simple.Interface_Description;
   pragma Inline (If_Desc);
   --  Interface description for SOA object adapter.

private
   use MOMA.Types;

   type Object is new PolyORB.Minimal_Servant.Servant with record
      Self_Ref : PolyORB.References.Ref;
      Message_Queue : Queue_Acc;
      Handler_Procedure : Handler := null;
      Notifier_Procedure : Notifier := null;
      Behavior : MOMA.Types.Call_Back_Behavior := None;
   end record;

   function Get_Parameter_Profile (Method : String)
     return PolyORB.Any.NVList.Ref;
   --  Parameters part of the interface description.

   function Get_Result_Profile (Method : String)
     return PolyORB.Any.Any;
   --  Result part of the interface description.

   procedure Handle (Self    : access Object;
                     Message : PolyORB.Any.Any);
   --  Execute the Handler procedure.
   --  Called when receiving a Handle request.

   procedure Notify (Self : access Object);
   --  Execute the Notifier procedure.
   --  Called when receiving a Notify request.

   procedure Register_To_Queue (Self : access Object);
   --  Register the Message_Handler or change the Behavior,
   --  via a Request to the actual queue,

end MOMA.Provider.Message_Handler;
