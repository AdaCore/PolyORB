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
--  the actual message consumer servant when a message is received : this
--  Request can either be Handle (then the message can not be recovered by a
--  call to the Message_Consumer's Receive and has to be treated by the Handle
--  procedure), or Notify (then the message stays in the pool).

--  $Id$

with MOMA.Messages;
with MOMA.Message_Consumers;
with MOMA.Types;

with PolyORB.Minimal_Servant;
with PolyORB.Requests;
with PolyORB.Obj_Adapters.Simple;
with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.References;

package MOMA.Provider.Message_Handler is

   use MOMA.Message_Consumers;
   use MOMA.Types;

   type Object is new PolyORB.Minimal_Servant.Servant with private;
   --  Self_Ref
   --  Message_Cons
   --  Handler_Procedure
   --  Notifier_Procedure
   --  Behavior
   --  XXX Add comments about the various attributes

   type Object_Acc is access Object;

   type Handler is access procedure (
      Self : access Object;
      Message : MOMA.Messages.Message'Class);
   --  The procedure to be called when a message is received, if the behavior
   --  is Handle.

   type Notifier is access procedure (
      Self : access Object);
   --  The procedure to be called when a message is received, if the behavior
   --  is Notify.

   procedure Initialize (Self                : in out Object_Acc;
                         Message_Cons        : Message_Consumer_Acc;
                         Self_Ref            : PolyORB.References.Ref;
                         Notifier_Procedure  : Notifier := null;
                         Handler_Procedure   : Handler := null;
                         Behavior            : MOMA.Types.Call_Back_Behavior
                                                  := None);
   --  Initialize the Message_Handler and return its Reference.
   --  If the behavior is Handle and no Handler_Procedure is provided, the
   --  incoming messages will be lost.

   procedure Invoke (Self : access Object;
                     Req  : in     PolyORB.Requests.Request_Access);
   --  Message_Handler servant skeleton.

   procedure Set_Behavior (
      Self           : access Object;
      New_Behavior   : in MOMA.Types.Call_Back_Behavior);
   --  Set the Behavior. A request is sent to the actual servant if the
   --  behavior has changed.

   procedure Set_Handler (
      Self                    : access Object;
      New_Handler_Procedure   : in Handler;
      Handle_Behavior         : Boolean := False);
   --  Associate a Handler procedure to the Message Handler.
   --  Replace the current Handler procedure.
   --  The behavior is set to Handle if Handle_Behavior is true.

   procedure Set_Notifier (
      Self                    : access Object;
      New_Notifier_Procedure  : in Notifier;
      Notify_Behavior         : Boolean := False);
   --  Symmetric of Set_Handler.

   procedure Template_Handler (
      Self     : access Object;
      Message  : MOMA.Messages.Message'Class);

   procedure Template_Notifier (
      Self : access Object);
   --  Templates for handler and notifier procedures.

   function If_Desc
     return PolyORB.Obj_Adapters.Simple.Interface_Description;
   pragma Inline (If_Desc);
   --  Interface description for SOA object adapter.

private
   type Object is new PolyORB.Minimal_Servant.Servant with record
      Self_Ref             : PolyORB.References.Ref;
      Message_Cons         : Message_Consumer_Acc;
      Handler_Procedure    : Handler := null;
      Notifier_Procedure   : Notifier := null;
      Behavior             : MOMA.Types.Call_Back_Behavior := None;
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

   procedure Register_To_Servant (Self : access Object);
   --  Register the Message_Handler or change the Behavior,
   --  via a Request to the actual servant.

end MOMA.Provider.Message_Handler;
