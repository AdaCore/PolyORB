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
with PolyORB.Call_Back;

package MOMA.Message_Consumers.Queues is

   use Ada.Real_Time;

   type Queue is new Message_Consumer with null record;

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

   function Receive (Self : Queue;
                     CBH : access PolyORB.Call_Back.Call_Back_Handler)
                     return MOMA.Messages.Message'Class;
   --  Get next message from the pool if it is non empty; otherwise the call
   --  is blocking until a new message is received by the pool.
   --  The message after the next will be signalled via the Call_Back_Handler.
   --  The Call_Back_Handler should be used only once and destroyed afterwards.
   --  This function should be used when the pool is not empty (e.g. after a
   --  callback).
   --  XXX not implemented.

   procedure Set_CBH (Self : Queue;
                      CBH : access PolyORB.Call_Back.Call_Back_Handler);
   --  Associates a Call_back_Handler to the pool.
   --  Next message will be signalled via the Call_Back_Handler.
   --  The Call_Back_Handler should be used only once and destroyed afterwards.
   --  XXX not implemented.

end MOMA.Message_Consumers.Queues;
