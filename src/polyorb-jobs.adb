------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         P O L Y O R B . J O B S                          --
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

--  $Id$

with Ada.Unchecked_Deallocation;

package body PolyORB.Jobs is

   procedure Free (X : in out Job_Access)
   is
      procedure Job_Free is new Ada.Unchecked_Deallocation
        (Job'Class, Job_Access);
   begin
      Job_Free (X);
   end Free;

   procedure Free is new Ada.Unchecked_Deallocation
     (Queue_Element, Queue_Element_Access);

   function Create_Queue return Job_Queue_Access
   is
      Q : constant Job_Queue_Access := new Job_Queue;
   begin
      Q.First := null;
      Q.Last  := null;

      return Q;
   end Create_Queue;

   procedure Queue_Job
     (Q : access Job_Queue;
      J : Job_Access)
   is
      E : Queue_Element_Access
        := new Queue_Element'(Next => null, Job  => J);
   begin
      if Q.Last = null then
         pragma Assert (Q.First = null);
         Q.First := E;
         Q.Last  := E;
      else
         Q.Last.Next := E;
         Q.Last := E;
      end if;
   end Queue_Job;

   function Empty (Q : access Job_Queue) return Boolean is
   begin
      return Q.First = null;
   end Empty;

   function Fetch_Job (Q : access Job_Queue) return Job_Access is
      First  : Queue_Element_Access := Q.First;
      Result : Job_Access := null;
   begin
      if First /= null then
         Result := First.Job;
         Q.First := First.Next;
         if Q.First = null then
            Q.Last := null;
         end if;

         Free (First);
      end if;

      return Result;
   end Fetch_Job;

end PolyORB.Jobs;
