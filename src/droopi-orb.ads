--  The ORB main loop and scheduler.

--  $Id$

with Sequences.Unbounded;

with Droopi.Channels;
with Droopi.Jobs;
with Droopi.Protocols;
with Droopi.Requests;
with Droopi.Servers;
with Droopi.Soft_Links;
with Droopi.Sockets;

package Droopi.ORB is

   use Droopi.Servers;

   ----------------------------------
   -- Abstract tasking policy type --
   ----------------------------------

   --  A tasking policy is a set of associations between
   --  certain events and the resources used to process them.
   --  These associations take the form of subprograms
   --  that take the event as input, create a job for
   --  its processing, and either create a new task for
   --  the execution of this job, or schedule it for execution
   --  by a general-purpose ORB task.

   type Tasking_Policy_Type is abstract tagged limited private;
   type Tasking_Policy_Access is access all Tasking_Policy_Type'Class;

   ---------------------
   -- A server object --
   ---------------------

   type ORB_Type (Tasking_Policy : access Tasking_Policy_Type'Class)
      is new Droopi.Servers.Server_Type with private;
   type ORB_Access is access all ORB_Type;

   ------------------------
   -- A decorated socket --
   ------------------------

   type Socket_Kind is
     (Invalid_Sk,
      Listening_Sk,
      Communication_Sk);

   type Active_Socket (Kind : Socket_Kind := Invalid_Sk) is record
      Socket   : Sockets.Socket_Type;
      Protocol : Protocols.Protocol_Access;

      case Kind is
         when Communication_Sk =>
            Session  : Protocols.Session_Access;
            Channel  : Channels.Channel_Access;
         when others =>
            null;
      end case;
   end record;

   type Event_Status is (No_Status, Connection_Closed);

   function Handle_Event
     (ORB : access ORB_Type;
      AS : Active_Socket)
     return Event_Status;
   --  Process events that have occurred on active socket AS, managed
   --  by server ORB.

   package Sock_Seqs is new Sequences.Unbounded (Active_Socket);
   subtype Sock_Seq is Sock_Seqs.Sequence;

   procedure Handle_New_Connection
     (P   : access Tasking_Policy_Type;
      ORB : ORB_Access;
      AS  : Active_Socket) is abstract;
   --  Create the necessary processing resources for newly-created
   --  communication channel C, and start dialog.

   procedure Handle_Request
     (P   : access Tasking_Policy_Type;
      ORB : ORB_Access;
      R   : Droopi.Requests.Request) is abstract;
   --  Create the necessary processing resources for the execution
   --  of request R, and start this execution.

   ------------------------------
   -- Server object operations --
   ------------------------------

   procedure Run
     (ORB : access ORB_Type; Exit_When : Exit_Condition_Access);
   --  Override inherited primitive.

   function Create_ORB
     (Tasking_Policy : Tasking_Policy_Access)
     return ORB_Access;
   --  Create a new ORB with the given Tasking_Policy and
   --  initialize it.

   procedure Run
     (ORB            : access ORB_Type;
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

   function Work_Pending (ORB : access ORB_Type) return Boolean;
   --  Return True if, and only if, some ORB processing is
   --  pending.

   procedure Perform_Work (ORB : access ORB_Type);
   --  Perform one elementary ORB action and return.

   procedure Shutdown
     (ORB                 : access ORB_Type;
      Wait_For_Completion : Boolean := True);
   --  Shut down ORB. If Wait_For_Completion is True, do
   --  not return before the shutdown is completed.

   procedure Insert_Socket
     (ORB : access ORB_Type;
      AS  : Active_Socket);
   --  Insert socket S with kind K in the set of sockets monitored by O.

   procedure Delete_Socket
     (ORB : access ORB_Type;
      AS  : Active_Socket);
   --  Delete socket S from the set of sockets monitored by ORB.

private

   type Tasking_Policy_Type is abstract tagged limited null record;

   type ORB_Type (Tasking_Policy : access Tasking_Policy_Type'Class)
   is new Droopi.Servers.Server_Type with record

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
      --  for external events on ORB_Sockets.

      Selector : Sockets.Selector_Access;
      --  The selector object used to wait for an external event.

   end record;

end Droopi.ORB;
