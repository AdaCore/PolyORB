------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         P O L Y O R B . J O B S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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

--  Job management for ORB activities.

--  $Id$

package PolyORB.Jobs is

   pragma Preelaborate;

   ---------
   -- Job --
   ---------

   type Job is abstract tagged limited private;
   type Job_Access is access all Job'Class;
   --  A Job is any elementary activity that may
   --  be assigned to an ORB task to be entirely
   --  processed within one ORB loop iteration.

   procedure Run (J : access Job) is abstract;
   --  Execute the given Job. A task processes a Job
   --  by invoking its Run primitive.

   procedure Free (X : in out Job_Access);
   --  Deallocate X.all.

   ---------------
   -- Job_Queue --
   ---------------

   type Job_Queue is limited private;
   type Job_Queue_Access is access all Job_Queue;
   --  A queue of pending jobs.

   function Create_Queue return Job_Queue_Access;
   --  Create a new job queue.

   procedure Queue_Job
     (Q : access Job_Queue;
      J : Job_Access);
   --  Enter a pending Job into Q.

   function Empty (Q : access Job_Queue) return Boolean;
   --  True if, and only if, Q contains no pending Job.

   function Fetch_Job (Q : access Job_Queue) return Job_Access;
   --  Returns a pending Job and remove it from Q.
   --  null is return if Q is empty.

   --  The caller must ensure that all primitive operations
   --  of Job_Queue are called only from within a critical
   --  section.

private

   ---------
   -- Job --
   ---------

   type Job is abstract tagged limited null record;

   ----------------------------------------------
   -- Job_Queue, implemented as a simple FIFO. --
   ----------------------------------------------

   type Queue_Element;
   type Queue_Element_Access is access Queue_Element;

   type Queue_Element is record
      Next : Queue_Element_Access;
      Job  : Job_Access;
   end record;

   type Job_Queue is limited record
      First, Last : Queue_Element_Access;
   end record;

end PolyORB.Jobs;
