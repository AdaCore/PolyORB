------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . L O C K E D _ Q U E U E                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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

--  This package implements a basic thread-safe bounded queue.

--  $Id$

with PolyORB.Soft_Links;

generic

   type Queue_Element is private;

package PolyORB.Locked_Queue is

   pragma Elaborate_Body;

   type Queue is limited private;

   procedure Create
     (Q         :    out Queue;
      Max_Count : in     Positive);
   --  Creates a queue with a limitation of Max_Count individuals.

   procedure Destroy
     (Q : in out Queue);
   --  Destroys a queue and frees memory.

   procedure Add
     (Q : in out Queue;
      E : in     Queue_Element);
   --  Appends an element to the end of the queue.
   --  This call is blocking when the queue is full.
   --
   --  ??? : This is a function that needs to be modified when
   --        adding the notion of priority to queue : request should be
   --        inserted in the queue with regard to their priority and
   --        not necessarily at the end.
   --
   --  ??? : A function Priority_Of (Element) return Natural should
   --        also be added to the generic part.

   procedure Get_Head
     (Q : in out Queue;
      E :    out Queue_Element);
   --  Removes the first element from the queue and returns it.
   --  This call is blocking when the queue is empty.


   function Get_Count
     (Q : Queue)
     return Natural;
   --  Returns the number of elements currently in the queue.

   function Get_Max_Count
     (Q : Queue)
     return Positive;
   --  Returns the total size of the queue.

private

   type Queue_Element_Access is access Queue_Element;

   type Queue_Node;

   type Queue_Node_Access is access Queue_Node;

   type Queue_Node is record
      Element : Queue_Element_Access;
      Next    : Queue_Node_Access;
   end record;

   type Queue is record
      Max_Count  : Positive;

      State_Lock : PolyORB.Soft_Links.Mutex_Access;
      --  This locks the global state of the queue, and should be
      --  taken when modifying First, Last and Count fields.

      Full_Lock  : PolyORB.Soft_Links.Mutex_Access;
      --  This lock is taken when the queue is full.

      Empty_Lock : PolyORB.Soft_Links.Mutex_Access;
      --  This lock is taken when the queue is empty.

      First      : Queue_Node_Access := null;
      Last       : Queue_Node_Access := null;
      Count      : Natural := 0;
   end record;

end PolyORB.Locked_Queue;
