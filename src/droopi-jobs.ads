--  Job management for ORB activities.
--  $Id$

package Droopi.Jobs is

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

   ---------------
   -- Job_Queue --
   ---------------

   type Job_Queue is limited private;
   type Job_Queue_Access is access all Job_Queue;
   --  A queue of pending jobs.

   procedure Create (Q : out Job_Queue_Access);
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

end Droopi.Jobs;
