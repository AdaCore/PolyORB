------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         P O L Y O R B . J O B S                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2017, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with PolyORB.Utils.Unchecked_Deallocation;

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
      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => Job'Class,
         Name => Job_Access);
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
