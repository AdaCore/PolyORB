--  The ORB main loop and scheduler.

--  $Id$

with Droopi.Asynchronous_Events;
with Droopi.Jobs;
with Droopi.Soft_Links;

package Droopi.ORB is

   pragma Preelaborate;

   type ORB is limited private;
   type ORB_Access is access all ORB;

   procedure Create (O : out ORB_Access);
   --  Create a new ORB instance.

   type Exit_Condition_Access is access all Boolean;

   subtype AES_Access is
     Droopi.Asynchronous_Events.Asynchronous_Event_Source_Access;

   procedure Run
     (O              : access ORB;
      Exit_Condition : Exit_Condition_Access := null;
      Blocker        : AES_Access := null);
   --  Execute the ORB until:
   --    - Exit_Condition.all becomes true
   --      (if Exit_Condition /= null), or
   --    - Shutdown is called on this ORB.

   --  This is executed by ORB tasks (with Exit_Condition = null)
   --  and is entered by user tasks that need to wait for an event
   --  to occur in the ORB (such tasks must execute the ORB when
   --  the threading policy is 'no threads').

   --  If Blocker is not null, then this is an ORB task which
   --  must wait for event on this asynchronous event source.

   function Work_Pending (O : access ORB) return Boolean;
   --  Return True if, and only if, some ORB processing is
   --  pending.

   procedure Perform_Work (O : access ORB);
   --  Perform one elementary ORB action and return.

   procedure Shutdown
     (O                   : access ORB;
      Wait_For_Completion : Boolean := True);
   --  Shut down ORB O. If Wait_For_Completion is True, do
   --  not return before the shutdown is completed.

private

   type ORB is limited record
      Shutdown   : Boolean := False;
      --  Set to True when ORB shutdown has been requested.

      Job_Queue  : Droopi.Jobs.Job_Queue_Access;
      --  The queue of jobs to be processed by ORB tasks.

      Idle_Tasks : Droopi.Soft_Links.Barrier_Access;
      --  Idle ORB task wait on this barrier.

      Polling    : Boolean;
      --  True if, and only if, an ORB task is blocked
      --  waiting for an external event.

   end record;

end Droopi.ORB;
