------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB CORBA SERVICES                         --
--                                                                          --
--                                                                          --
--                            PRIORITY_QUEUE.ADS                            --
--                                                                          --
--                                 S P E C                                  --
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
with CORBA;
with PolyORB.Tasking.Semaphores;


--  This package provides a priority-based queue, for use with
--  a modified COS Event channel.
--  The data in the queue is of type CORBA.Any, with an associated
--  priority between -32767 and +32767. In order to provide
--  performance independent of the number of elements in the queue,
--  and to make possible use of different element discard policies,
--  each queue element is linked 6 ways, to its immediate succesor
--  and predecessor in the queue, to its predecessor and
--  successor in arrival order, to the last element in the queue
--  of the same priority, and to the last element of higher
--  priority. This unfortunately makes the insertion and extraction
--  procedures somewhat complicated. Procedures are provided for
--  insertion of a new element, and extraction of the highest-priority
--  element in the queue.


package Priority_Queue is

   --  Type to contain the actual queue
   type Priority_Queue_Type (MaxElt : Integer) is private;
   type Priority_Queue_Access is access all Priority_Queue_Type;

   --  Possible values for a priority
   subtype Priority_Value is Integer range -32767 .. 32767;

   --  Possible values for Order and Discard policies
   --  according to COS Notification specification
   type Discard_Policy_Values is (Fifo, Lifo, Priority);
   type Order_Policy_Values is (Fifo, Priority);

   --  Allowable values for Factor :
   --  Controls sub-division of complete priority range
   subtype Factor_Range is Integer range 0 .. 16;

   --  Create a priority queue
   procedure Create (PQ : out Priority_Queue_Access;
      The_Factor : in Factor_Range);

   --  Destroy a priority queue
   procedure Destroy (PQ : in out Priority_Queue_Access);

   --  Insert a new element in the queue
   procedure Insert
    (PQ : in out Priority_Queue_Access;
     Data : in CORBA.Any;
     Prio : in Priority_Value);

   --  Retrieve the highest-priority element in the queue
   procedure Pop (PQ : in out Priority_Queue_Access; Data : out CORBA.Any);

   --  Set the Order policy
   procedure Set_OrderPolicy (PQ : in out Priority_Queue_Access;
      Policy : in Order_Policy_Values);

   --  Set the Maximum Queue Size
   procedure Set_MaxQueueSize (PQ : in out Priority_Queue_Access;
      Size : in Integer);

private
   use PolyORB.Tasking.Semaphores;

   --  A single queue element
   type Queue_Elt;
   type Queue_Elt_Ptr is access Queue_Elt;
   type Queue_Elt is record
      --  pointer to next queue element
      Next : Queue_Elt_Ptr;
      --  pointer to most recently arrived element of same priority
      Last : Queue_Elt_Ptr;
      --  pointer to previous queue element
      Prev : Queue_Elt_Ptr;
      --  pointer to next queue element, in order of arrival
      NextTime : Queue_Elt_Ptr;
      --  pointer to previous queue element, in order of arrival
      PrevTime : Queue_Elt_Ptr;
      --  pointer to last element of strictly higher priority
      Last_Higher : Queue_Elt_Ptr;
      --  the event data
      Data : CORBA.Any;
      --  the event's priority
      P : Priority_Value;
   end record;

   --  Array type containing the different queues
   type Priority_Queue_Array is array (Integer range <>) of Queue_Elt_Ptr;

   type Priority_Queue_Type (MaxElt : Integer) is record
      --  The priority queues
      Priority_Queues : Priority_Queue_Array (0 .. MaxElt);
      --  Highest priority currently present in the queue
      Highest_Priority : Integer := 0;
      --  Lowest priority currently present in the queue
      Lowest_Priority : Integer := MaxElt;
      --  Semaphore controling access to the queue
      Queue_Lock : Semaphore_Access := null;
      --  Value controlling the size of priority range subdivisions
      Factor : Factor_Range := 0;
      --  Maximum number of events allowed in the queue, -1 if unlimited
      MaxSize : Integer := -1;
      --  Pointer to earliest arrived element
      FirstTime : Queue_Elt_Ptr := null;
      --  Pointer to most recently arrived element
      LastTime : Queue_Elt_Ptr := null;
      --  Current Discard policy
      Discard : Discard_Policy_Values := Lifo;
      --  Current Order policy
      Order : Order_Policy_Values := Fifo;
   end record;

   --  if the queue is full, this procedure decides which element
   --  to eliminate to make more room.
   procedure Discard (PQ : in out Priority_Queue_Access; Result : out Boolean);

end Priority_Queue;
