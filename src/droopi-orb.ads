--  The ORB main loop and scheduler.

--  $Id$

with Sequences.Unbounded;

with Droopi.Jobs;
with Droopi.Soft_Links;
with Droopi.Sockets;
with Droopi.Tasking_Policies;

package Droopi.ORB is

   pragma Elaborate_Body;

   ---------------------
   -- A server object --
   ---------------------

   type ORB is limited private;
   type ORB_Access is access all ORB;

   ------------------------
   -- A decorated socket --
   ------------------------

   type Socket_Kind is
     (Invalid_SK,
      Listening_SK,
      Communication_SK);

   type Active_Socket (Kind : Socket_Kind := Invalid_SK) is record
      Socket   : Sockets.Socket_Type;
      --  Protocol : Protocol_Access;
   end record;

   procedure Handle_Event (O : access ORB; AS : Active_Socket);
   --  Process events that have occurred on active socket AS, managed
   --  by server O.

   package Sock_Seqs is new Sequences.Unbounded (Active_Socket);
   subtype Sock_Seq is Sock_Seqs.Sequence;

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

   procedure Insert_Socket
     (O : access ORB;
      S : Sockets.Socket_Type;
      K : Socket_Kind);
   --  Insert socket S with kind K in the set of sockets monitored by O.

   procedure Delete_Socket
     (O : access ORB;
      S : Sockets.Socket_Type);
   --  Delete socket S from the set of sockets monitored by O.

private

   type ORB is limited record

      -----------------------
      -- Server parameters --
      -----------------------

      Tasking_Policy : Tasking_Policies.Tasking_Policy_Access;

      ------------------
      -- Server state --
      ------------------

      Shutdown   : Boolean := False;
      --  Set to True when ORB shutdown has been requested.

      Job_Queue  : Jobs.Job_Queue_Access;
      --  The queue of jobs to be processed by ORB tasks.

      Idle_Tasks : Soft_Links.Barrier_Access;
      --  Idle ORB task wait on this barrier.

      ORB_Sockets : Sock_Seq;
      --  The set of transport endpoints to be monitored
      --  by ORB tasks.

      Polling : Boolean;
      --  True if, and only if, one task is blocked waiting
      --  for external events on Monitored_Sockets.

      Selector : Sockets.Selector_Access;
      --  The selector object used to wait for an external event.

   end record;

end Droopi.ORB;
