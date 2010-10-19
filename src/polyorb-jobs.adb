------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         P O L Y O R B . J O B S                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2009, Free Software Foundation, Inc.          --
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

pragma Ada_2005;

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
      Selector : access function (J : Job'Class) return Boolean := null)
      return Job_Access
   is
      use Job_Queues;

      Result : Job_Access;
      It     : Iterator := First (Q.Contents);
   begin
      while not Last (It) loop
         if Selector = null or else Selector (Value (It).all.all) then
            Result := Job_Access (Value (It).all);
            Remove (Q.Contents, It);
            return Result;
         end if;
         Next (It);
      end loop;

      return null;
   end Fetch_Job;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Job_Access) is
      procedure Free is new Ada.Unchecked_Deallocation (Job'Class, Job_Access);
   begin
      Free (X);
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
      return Job_Queues.Is_Empty (Q.Contents);
   end Is_Empty;

   ---------------
   -- Queue_Job --
   ---------------

   procedure Queue_Job
     (Q : access Job_Queue;
      J : Job_Access) is
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
