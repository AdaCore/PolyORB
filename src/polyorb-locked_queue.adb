------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . L O C K E D _ Q U E U E                  --
--                                                                          --
--                                 B o d y                                  --
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

with Unchecked_Deallocation;

with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);

package body PolyORB.Locked_Queue is

   use PolyORB.Log;
   use PolyORB.Soft_Links;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.locked_queue");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Free is new Unchecked_Deallocation
     (Queue_Node, Queue_Node_Access);
   procedure Free is new Unchecked_Deallocation
     (Queue_Element, Queue_Element_Access);

   ---------------
   -- Get_Count --
   ---------------

   function Get_Count
     (Q : Queue)
     return Natural
   is
      N : Natural;
   begin
      Enter (Q.State_Lock);
      N := Q.Count;
      Leave (Q.State_Lock);
      return N;
   end Get_Count;

   -------------------
   -- Get_Max_Count --
   -------------------

   function Get_Max_Count
     (Q : Queue)
     return Positive
   is
   begin
      return Q.Max_Count;
   end Get_Max_Count;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Q : in out Queue)
   is
      Next : Queue_Node_Access;
   begin
      Enter (Q.State_Lock);
      while Q.First /= null loop
         Next := Q.First.Next;
         Free (Q.First);
         Q.First := Next;
      end loop;
      Leave (Q.State_Lock);

      Destroy (Q.State_Lock);
      Destroy (Q.Full_Lock);
      Destroy (Q.Empty_Lock);
   end Destroy;

   ------------
   -- Create --
   ------------

   procedure Create
     (Q         :    out Queue;
      Max_Count : in     Positive)
   is
   begin
      Create (Q.State_Lock);

      Enter (Q.State_Lock);
      Q.Max_Count := Max_Count;
      Q.Count := 0;
      Create (Q.Empty_Lock);
      Create (Q.Full_Lock);
      Create (Q.State_Lock);
      Leave (Q.State_Lock);

      Enter (Q.Empty_Lock);
   end Create;

   ---------
   -- Add --
   ---------

   procedure Add
     (Q : in out Queue;
      E : in     Queue_Element)
   is
   begin
      Enter (Q.Full_Lock);

      Enter (Q.State_Lock);
      if Q.First = null then
         Q.Last := new Queue_Node'
           (Element => new Queue_Element'(E),
            Next    => null);
         Q.First := Q.Last;
      else
         Q.Last.Next := new Queue_Node'
           (Element => new Queue_Element'(E),
            Next    => null);
         Q.Last := Q.Last.Next;
      end if;
      Q.Count := Q.Count + 1;
      Leave (Q.State_Lock);

      if Q.Count = 1 then
         Leave (Q.Empty_Lock);
      end if;

      if Q.Count /= Q.Max_Count then
         Leave (Q.Full_Lock);
      end if;
   end Add;

   --------------
   -- Get_Head --
   --------------

   procedure Get_Head
     (Q : in out Queue;
      E :    out Queue_Element)
   is
   begin
      Enter (Q.Empty_Lock);

      --  When execution reaches this, necessarily Q.First /= null.

      Enter (Q.State_Lock);

      declare
         Old_First : Queue_Node_Access := Q.First;
      begin
         E := Q.First.Element.all;
         Q.First := Q.First.Next;
         Free (Old_First);
      end;

      Q.Count := Q.Count - 1;
      Leave (Q.State_Lock);

      --  When execution reaches this, necessarily the queue is not full.

      if Q.Count = Q.Max_Count - 1 then
         Leave (Q.Full_Lock);
      end if;

      if Q.Count > 0 then
         Leave (Q.Empty_Lock);
      end if;

   end Get_Head;

end PolyORB.Locked_Queue;
