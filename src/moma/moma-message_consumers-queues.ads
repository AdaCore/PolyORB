------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        M O M A . M E S S A G E _ C O N S U M E R S . Q U E U E S         --
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

--  Derivation of Message_Consumers for Queues.

--  $Id$

with Ada.Real_Time;

with MOMA.Messages;

package MOMA.Message_Consumers.Queues is

   use Ada.Real_Time;

   type Queue is new Message_Consumer with private;

   type Handler is access procedure (Message_Queue : Queue;
                                     Message : MOMA.Messages.Message'Class);
   --  The procedure to be called when a message is received.

   type Notifier is access procedure (Message_Queue : Queue);

   function Get_Queue (Self : Queue) return MOMA.Destinations.Queue;
   --  XXX not implemented.

   function Receive (Self : Queue) return MOMA.Messages.Message'Class;
   --  Get next message from the pool if it is non empty; otherwise the call
   --  is blocking until a new message is received by the pool.
   --  XXX not all cases are tested !

   function Receive (Timeout : Time) return MOMA.Messages.Message;
   --  Get next message from the pool if it is non empty; otherwise will
   --  wait 'Timeout' until a new message arrives.
   --  XXX not implemented.

   function Receive_No_Wait return MOMA.Messages.Message;
   --  Get next message from the pool if it is non empty; exit otherwise.
   --  XXX not implemented.

   procedure Set_Handler (Self : in out Queue;
                          New_Handler_Procedure : in Handler);
   --  Associates a Handler to the pool.
   --  The actual queue will call the Handler when receiving a new message.

   procedure Set_Notifier (Self : in out Queue;
                           New_Notifier_Procedure : in Notifier);
   --  Associates a Handler to the pool.
   --  The actual queue will call the Handler when receiving a new message.

   procedure Handle (Self : Queue;
                     Message : MOMA.Messages.Message'Class);
   --  Execute the Handler procedure.
   --  Called by the Message_Handler object when receiving a Handle
   --  request.

   procedure Notify (Self : Queue);
   --  Execute the Notifier procedure.
   --  Called by the Message_Handler object when receiving a Notify
   --  request.

private
   type Queue is new Message_Consumer with record
      Has_Initialized_Message_Handler : Boolean := False;
      Handler_Procedure : Handler := null;
      Notifier_Procedure : Notifier := null;
   end record;

   procedure Initialize_Message_Handler (Self : in out Queue);
   --  Initialize a new Message_Handler object that can receive requests
   --  when a message is received by the actual queue.
   --  XXX not implemented.
end MOMA.Message_Consumers.Queues;
