------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                M O M A . M E S S A G E _ H A N D L E R S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
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

--  A Message_Handler object is the client view of the message handling
--  process. It is the facade used to define the callback behavior and
--  procedures, and provides templates ; it contains the stub to access
--  Message_Handler servants (see MOMA.Provider for more details).

--  NOTE: A MOMA client must use only this package to get a callback for the
--  messages it receives.

with MOMA.Messages;
with MOMA.Message_Consumers;
with MOMA.Sessions;
with MOMA.Types;

with PolyORB.Annotations;

package MOMA.Message_Handlers is

   use MOMA.Types;

   type Message_Handler is private;
   --  Self_Ref is the reference to the Message_Handler Servant.
   --  Message_Cons is the Message_Consumer associated to the Message_Handler.
   --  Behavior is the callback behavior, which changes are passed to the
   --  Message_Consumer actual servant.
   --  Handler_Procedure is the procedure called when a message is received by
   --  the Message_Consumer actual Servant when the behavior is Handle.
   --  Notifier_Procedure is the procedure called when a message is received
   --  by the Message_Consumer actual Servant when the behavior is Notify.
   --  Call_Back_Data contains callback data in the form of Notes than can be
   --  set by the client. The actual type of Notes is up to the client.

   type Message_Handler_Acc is access Message_Handler;

   type Handler is access procedure
     (Self    : access Message_Handler;
      Message :        MOMA.Messages.Message'Class);
   --  The procedure to be called when a message is received, if the behavior
   --  is Handle.

   type Notifier is access procedure (Self : access Message_Handler);
   --  The procedure to be called when a message is received, if the behavior
   --  is Notify.

   function Create_Handler
     (Session             : MOMA.Sessions.Session;
      Message_Cons        : MOMA.Message_Consumers.Message_Consumer_Acc;
      Notifier_Procedure  : Notifier := null;
      Handler_Procedure   : Handler := null;
      Behavior            : MOMA.Types.Call_Back_Behavior := None)
      return MOMA.Message_Handlers.Message_Handler_Acc;
   --  Create a Message Handler associated to the specified Message consumer.
   --  If the behavior is Handle and no Handler_Procedure is provided, the
   --  incoming messages will be lost.

   procedure Get_Call_Back_Data
     (Self : access Message_Handler;
      Data :    out PolyORB.Annotations.Note'Class);
   --  Retrieve Call_Back Data for use in Handler or Notifier procedure.

   function Get_Consumer
     (Self : access Message_Handler)
     return MOMA.Message_Consumers.Message_Consumer;

   function Get_Handler
     (Self : access Message_Handler)
     return Handler;
   --  Get the Handler procedure.

   function Get_Notifier
     (Self : access Message_Handler)
     return Notifier;
   --  Get the Notifier procedure.

   procedure Set_Behavior
     (Self           : access Message_Handler;
      New_Behavior   : MOMA.Types.Call_Back_Behavior);
   --  Set the Behavior. A request is sent to the actual servant if the
   --  behavior has changed.

   procedure Set_Call_Back_Data
     (Self : access Message_Handler;
      Data :        PolyORB.Annotations.Note'Class);
   --  Set Call_Back Data for use in Handler or Notifier procedure.

   procedure Set_Handler
     (Self                    : access Message_Handler;
      New_Handler_Procedure   : Handler;
      Handle_Behavior         :        Boolean := False);
   --  Associate a Handler procedure to the Message Handler.
   --  Replace the current Handler procedure.
   --  The behavior is set to Handle if Handle_Behavior is true.

   procedure Set_Notifier
     (Self                    : access Message_Handler;
      New_Notifier_Procedure  : Notifier;
      Notify_Behavior         :        Boolean := False);
   --  Associate a Notifier procedure to the Message Handler.
   --  Replace the current Handler procedure.
   --  The behavior is set to Handle if Notify_Behavior is true.

   procedure Template_Handler
     (Self     : access Message_Handler;
      Message  :        MOMA.Messages.Message'Class);

   procedure Template_Notifier (Self : access Message_Handler);
   --  Templates for handler and notifier procedures.

private

   type Message_Handler is record
      Servant_Ref          : MOMA.Types.Ref;
      Message_Cons         : MOMA.Message_Consumers.Message_Consumer_Acc;
      Handler_Procedure    : Handler := null;
      Notifier_Procedure   : Notifier := null;
      Behavior             : MOMA.Types.Call_Back_Behavior := None;
      Call_Back_Data       : aliased PolyORB.Annotations.Notepad;
   end record;

end MOMA.Message_Handlers;
