------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         P O L Y O R B . J O B S                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2004 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body PolyORB.Jobs is

   ------------------
   -- Create_Queue --
   ------------------

   function Create_Queue return Job_Queue_Access is
   begin
      return new Job_Queue;
   end Create_Queue;

   ---------------
   -- Fetch_Job --
   ---------------

   function Fetch_Job
     (Q        : access Job_Queue;
      Selector :        Job_Selector := Any_Job)
      return Job_Access
   is
      use Job_Queues;

      Result : Job_Access;
   begin
      if Is_Empty (Q) then
         return null;
      end if;

      --  If no selector is provided, extract the first element

      if Selector = null then
         Extract_First (Q.Contents, Result);
         return Result;
      end if;

      --  else return the first selected element

      declare
         It     : Iterator := First (Q.Contents);

      begin
         while not Last (It) loop
            if Selector (Job_Access (Value (It).all)) then
               Result := Job_Access (Value (It).all);
               Remove (Q.Contents, It);
               return Result;
            end if;

            Next (It);
         end loop;

         return null;
      end;
   end Fetch_Job;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Job_Access) is
      procedure Do_Free is new Ada.Unchecked_Deallocation
        (Job'Class, Job_Access);
   begin
      Do_Free (X);
   end Free;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty
     (Q : access Job_Queue)
     return Boolean
   is
      use type Job_Queues.List;
   begin
      return Q.Contents = Job_Queues.Empty;
   end Is_Empty;

   ---------------
   -- Queue_Job --
   ---------------

   procedure Queue_Job
     (Q : access Job_Queue;
      J :        Job_Access) is
   begin
      Job_Queues.Append (Q.Contents, J);
   end Queue_Job;

   ------------
   -- Length --
   ------------

   function Length (Q : access Job_Queue) return Natural is
   begin
      return Job_Queues.Length (Q.Contents);
   end Length;

end PolyORB.Jobs;
