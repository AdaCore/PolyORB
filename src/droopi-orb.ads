--  The ORB main loop and scheduler.

--  $Id$

with Sequences.Unbounded;

with Droopi.Asynch_Ev;
with Droopi.Filters;
with Droopi.Jobs;
with Droopi.Obj_Adapters;
with Droopi.Requests;
with Droopi.Schedulers;
with Droopi.Soft_Links;
with Droopi.Transport;

package Droopi.ORB is

   use Droopi.Asynch_Ev;
   use Droopi.Schedulers;
   use Droopi.Transport;

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
      is new Droopi.Schedulers.Server_Type with private;
   type ORB_Access is access all ORB_Type;

   package Monitor_Seqs is new Sequences.Unbounded
     (Asynch_Ev.Asynch_Ev_Monitor_Access);
   subtype Monitor_Seq is Monitor_Seqs.Sequence;

   type Active_Connection is record
      AES : Asynch_Ev_Source_Access;
      TE  : Transport_Endpoint_Access;
   end record;

   procedure Handle_New_Server_Connection
     (P   : access Tasking_Policy_Type;
      ORB : ORB_Access;
      C   : Active_Connection) is abstract;
   --  Create the necessary processing resources for newly-created
   --  communication endpoint AS on server side.

   procedure Handle_New_Client_Connection
     (P   : access Tasking_Policy_Type;
      ORB : ORB_Access;
      C   : Active_Connection) is abstract;
   --  Create the necessary processing resources for newly-created
   --  communication endpoint AS on client side.

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

   procedure Create (ORB : in out ORB_Type);
   --  Initialize a newly-allocated ORB object.

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

   procedure Register_Access_Point
     (ORB   : access ORB_Type;
      TAP   : Transport_Access_Point_Access;
      Chain : Filters.Factory_Chain_Access);
   --  Register a newly-created transport access point with
   --  ORB. When a connection is received on TAP, a filter
   --  chain is instanciated using Chain, and associated
   --  to the corresponding transport endpoint.

   type Endpoint_Role is (Client, Server);

   procedure Register_Endpoint
     (ORB   : access ORB_Type;
      TE    : Transport_Endpoint_Access;
      Chain : Filters.Factory_Chain_Access;
      Role  : Endpoint_Role);
   --  Register a newly-created transport endpoint with ORB.
   --  A filter chain is instanciated using Chain, and associated
   --  with TE.

   procedure Set_Object_Adapter
     (ORB : access ORB_Type;
      OA  : Obj_Adapters.Obj_Adapter_Access);
   --  Associate object adapter OA with ORB.
   --  Objects registered with OA become visible through
   --  ORB for external request invocation.

   function Object_Adapter (ORB : access ORB_Type)
     return Obj_Adapters.Obj_Adapter_Access;
   --  Return the object adapter associated with ORB.

   procedure Insert_Source
     (ORB : access ORB_Type;
      AES : Asynch_Ev_Source_Access);
   --  Insert AES in the set of asynchronous event sources
   --  monitored by ORB.

   procedure Delete_Source
     (ORB : access ORB_Type;
      AES : Asynch_Ev_Source_Access);
   --  Delete AES from the set of asynchronous event sources
   --  monitored by ORB.

   procedure Queue_Job
     (ORB : access ORB_Type;
      J   : Droopi.Jobs.Job_Access);

   procedure Queue_Request
     (ORB : access ORB_Type;
      R   : Droopi.Requests.Request_Access);
   --  Perform request R, then destroy it.

private

   type Tasking_Policy_Type is abstract tagged limited null record;

   type ORB_Type (Tasking_Policy : access Tasking_Policy_Type'Class)
   is new Droopi.Schedulers.Server_Type with record

      -----------------------------------
      -- Mutex for access to ORB state --
      -----------------------------------

      ORB_Lock : Soft_Links.Adv_Mutex_Access;

      ------------------
      -- Server state --
      ------------------

      Shutdown   : Boolean := False;
      --  Set to True when ORB shutdown has been requested.

      Job_Queue  : Jobs.Job_Queue_Access;
      --  The queue of jobs to be processed by ORB tasks.

      Idle_Tasks : Soft_Links.Watcher_Access;
      --  Idle ORB task wait on this watcher.

      Monitors : Monitor_Seq;
      --  The set of asynchronous event monitors to be watched
      --  by ORB tasks.

      Polling : Boolean;
      --  True if, and only if, one task is blocked waiting
      --  for external events on ORB_Sockets.

      Selector : Asynch_Ev.Asynch_Ev_Monitor_Access;
      --  The asynchronous event monitor on which this ORB is
      --  currently waiting for events.

      Obj_Adapter : Obj_Adapters.Obj_Adapter_Access;
      --  The object adapter that manages objects registered
      --  with this ORB.
   end record;

end Droopi.ORB;
