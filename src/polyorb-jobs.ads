------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         P O L Y O R B . J O B S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

--  Job management for ORB activities.

pragma Ada_2005;

with PolyORB.Utils.Chained_Lists;

package PolyORB.Jobs is

   pragma Preelaborate;

   ---------
   -- Job --
   ---------

   type Job is abstract tagged limited private;
   --  A Job is any elementary activity that may be assigned to an ORB task to
   --  be entirely processed within one ORB loop iteration.

   type Job_Access is access all Job'Class;
   procedure Free (X : in out Job_Access);

   procedure Run (J : not null access Job) is abstract;
   --  Execute the given Job. A task processes a Job by invoking its Run
   --  primitive.

   ---------------
   -- Job_Queue --
   ---------------

   type Job_Queue is limited private;
   type Job_Queue_Access is access all Job_Queue;
   --  A queue of pending jobs

   function Create_Queue return Job_Queue_Access;
   --  Create a new job queue

   procedure Queue_Job
     (Q : access Job_Queue;
      J : Job_Access);
   --  Enter a pending Job into Q.

   function Is_Empty (Q : access Job_Queue) return Boolean;
   --  True if, and only if, Q contains no pending Job

   function Fetch_Job
     (Q        : access Job_Queue;
      Selector : access function (J : Job'Class) return Boolean := null)
      return Job_Access;
   --  Returns a pending Job that matches Selector (i.e. such that
   --  Selector.all (Job) is true), and remove it from Q. Null is returned if
   --  no matching job exists. All jobs match a null Selector.

   --  The caller must ensure that all primitive operations of Job_Queue are
   --  called only from within a critical section.

   function Length (Q : access Job_Queue) return Natural;

private

   pragma Inline (Queue_Job);
   pragma Inline (Is_Empty);
   pragma Inline (Fetch_Job);
   pragma Inline (Length);

   type Job is abstract tagged limited null record;

   package Job_Queues is new PolyORB.Utils.Chained_Lists
     (Job_Access, Doubly_Chained => True);

   subtype Job_Queue_Internal is Job_Queues.List;

   type Job_Queue is limited record
     Contents : Job_Queue_Internal;
   end record;

end PolyORB.Jobs;
