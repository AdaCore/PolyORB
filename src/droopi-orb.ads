--  The ORB main loop and scheduler.

--  $Id$

with System.Garlic.Sockets;

with Sequences.Unbounded;

with Droopi.Jobs;
with Droopi.Soft_Links;

package Droopi.ORB is

   pragma Elaborate_Body;

   package Sk renames System.Garlic.Sockets;

   ---------------------
   -- A server object --
   ---------------------

   type ORB is limited private;
   type ORB_Access is access all ORB;

   ------------------------
   -- A decorated socket --
   ------------------------

   type Socket_Kind is
     (Invalid_Sk,
      Listening_Sk,
      Communication_Sk);

   type Active_Socket (Kind : Socket_Kind := Invalid_Sk) is record
      Socket   : Sk.Socket_Type;
      --  Protocol : Protocol_Access;
   end record;

   procedure Handle_Event (O : access ORB; AS : Active_Socket);
   --  Process events that have occurred on active socket AS, managed
   --  by server O.

   package Sk_Seqs is new Sequences.Unbounded (Active_Socket);
   subtype Sk_Seq is Sk_Seqs.Sequence;

   ------------------------------
   -- Server object operations --
   ------------------------------

   function Create_ORB return ORB_Access;
   --  Create a new ORB instance and initialize it.

   type Exit_Condition_Access is access all Boolean;

   procedure Run
     (O              : access ORB;
      Exit_Condition : Exit_Condition_Access := null;
      May_Poll       : Boolean := False);
   --  Execute the ORB until:
   --    - Exit_Condition.all becomes true
   --      (if Exit_Condition /= null), or
   --    - Shutdown is called on this ORB.

   --  This is executed by ORB tasks (with Exit_Condition = null)
   --  and is entered by user tasks that need to wait for an event
   --  to occur in the ORB (such tasks must execute the ORB when
   --  the threading policy is 'no threads').

   --  If May_Poll, then this task may suspend itself to wait
   --  for external events.

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

      Sockets : Sk_Seq;
      --  The set of transport endpoints to be monitored
      --  by ORB tasks.

      Polling : Boolean;
      --  True if, and only if, one task is blocked waiting
      --  for external events on Monitored_Sockets.

      Selector : Sk.Selector_Access;
      --  The selector object used to wait for an external event.

   end record;

end Droopi.ORB;
