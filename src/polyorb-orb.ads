------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . O R B                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2003 Free Software Fundation              --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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

--  The ORB core module: main loop and scheduler.
--  Role: * to coordinate operation of the various subsystems.
--        * to gateway asynchronous external events to the
--          syncrhonous messaging architecture used within PolyORB.

--  $Id$

with PolyORB.Asynch_Ev;
with PolyORB.Annotations;
with PolyORB.Binding_Data;
with PolyORB.Components;
with PolyORB.Filters;
with PolyORB.Jobs;
with PolyORB.Obj_Adapters;
with PolyORB.Objects;
with PolyORB.References;
with PolyORB.Requests;
with PolyORB.Sequences.Unbounded;
with PolyORB.Tasking.Advanced_Mutexes;
with PolyORB.Tasking.Watchers;
with PolyORB.Task_Info;
with PolyORB.Transport;

package PolyORB.ORB is

   package PAE renames PolyORB.Asynch_Ev;
   package PBD renames PolyORB.Binding_Data;
   package PC  renames PolyORB.Components;
   package PF  renames PolyORB.Filters;
   package PJ  renames PolyORB.Jobs;
   package PT  renames PolyORB.Transport;

   type Request_Job is new PJ.Job with private;

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

   ----------------------------------------
   -- Utility routines for jobs handling --
   ----------------------------------------

   function Duplicate_Request_Job
     (RJ : access PJ.Job'Class)
     return PJ.Job_Access;
   --  Create a copy of RJ, a Request_Job, so it can be stored
   --  for later execution.

   ---------------------
   -- A server object --
   ---------------------

   type ORB_Type
     (Tasking_Policy : access Tasking_Policy_Type'Class)
      is new PolyORB.Components.Component with private;
   type ORB_Access is access all ORB_Type;

   -------------------------------
   -- Tasking policy operations --
   -------------------------------

   type Active_Connection is record
      AES : PAE.Asynch_Ev_Source_Access;
      TE  : PT.Transport_Endpoint_Access;
   end record;

   --  Abstract primitives of Tasking_Policy_Type (need to
   --  be visible, RM 3.9.3(10)).

   procedure Handle_New_Server_Connection
     (P   : access Tasking_Policy_Type;
      ORB : ORB_Access;
      C   : Active_Connection) is abstract;
   --  Create the necessary processing resources for newly-created
   --  communication endpoint AS on server side.

   procedure Handle_Close_Server_Connection
     (P   : access Tasking_Policy_Type;
      TE  :        PT.Transport_Endpoint_Access) is abstract;
   --  Do necessary processing when a connection is closed

   procedure Handle_New_Client_Connection
     (P   : access Tasking_Policy_Type;
      ORB : ORB_Access;
      C   : Active_Connection) is abstract;
   --  Create the necessary processing resources for newly-created
   --  communication endpoint AS on client side.

   procedure Handle_Request_Execution
     (P   : access Tasking_Policy_Type;
      ORB : ORB_Access;
      RJ  : access Request_Job'Class)
      is abstract;
   --  Create the necessary processing resources for the execution
   --  of request execution job RJ, and start this execution.
   --  RJ is freed after it is called.

   procedure Idle
     (P   : access Tasking_Policy_Type;
      ORB : ORB_Access) is abstract;
   --  Called by a task that has nothing to do in order
   --  to wait until there may be anything to do.
   --  The calling task must hold ORB_Lock before calling Idle;
   --  the tasking policy shall release it while the task is
   --  idling, and re-assert it before Idle returns.

   procedure Queue_Request_To_Handler
     (P   : access Tasking_Policy_Type;
      ORB : ORB_Access;
      Msg : PolyORB.Components.Message'Class) is abstract;
   --  Assign the handling of a Request (i.e. an upcall to
   --  an application object) to the appropriate task.
   --  XXX It looks like this is implemented in exactly identical terms
   --  by all existing ORB tasking policies (as of 20021212). This should
   --  be removed from the TP interface.

   ------------------------------
   -- Server object operations --
   ------------------------------

   type Boolean_Access is access all Boolean;
   type Task_Info_Access_Access is
     access all PolyORB.Task_Info.Task_Info_Access;

   type Exit_Condition_T is record
      Condition : Boolean_Access;
      Task_Info : Task_Info_Access_Access;
   end record;

   procedure Create (ORB : in out ORB_Type);
   --  Initialize a newly-allocated ORB object.

   procedure Run
     (ORB            : access ORB_Type;
      Exit_Condition : Exit_Condition_T := (null, null);
      May_Poll       : Boolean := False);
   --  Execute the ORB until:
   --    - Exit_Condition.Condition.all becomes true
   --      (if Exit_Condition.Condition /= null), or
   --    - Shutdown is called on this ORB.

   --  This procedure is executed by ORB tasks (with
   --  Exit_Condition.Condition = null) and is entered by user
   --  tasks that need to wait for a certain condition to occur
   --  (such tasks must execute the ORB when the threading policy
   --  is 'no threads').

   --  If Exit_Condition.Task_Info is not null, it is set on
   --  entry into Run to an access value that designates
   --  this task's Task_Info structure while it is executing
   --  ORB.Run.

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
      TAP   : PT.Transport_Access_Point_Access;
      Chain : PF.Factory_Access;
      PF    : PBD.Profile_Factory_Access);
   --  Register a newly-created transport access point with
   --  ORB. When a connection is received on TAP, a filter
   --  chain is instanciated using Chain, and associated
   --  to the corresponding transport endpoint.

   function Is_Profile_Local
     (ORB : access ORB_Type;
      P   : access Binding_Data.Profile_Type'Class)
     return Boolean;
   --  True iff P designates an object managed by this ORB.

   type Endpoint_Role is (Client, Server);

   procedure Register_Endpoint
     (ORB          : access ORB_Type;
      TE           :        PT.Transport_Endpoint_Access;
      Filter_Stack :        PF.Filter_Access;
      Role         :        Endpoint_Role);
   --  Register a newly-created transport endpoint with ORB.
   --  A filter chain is instanciated using Chain, and associated
   --  with TE.

   procedure Set_Object_Adapter
     (ORB : access ORB_Type;
      OA  : Obj_Adapters.Obj_Adapter_Access);
   --  Associate object adapter (OA) with ORB.
   --  Objects registered with OA become visible through
   --  ORB for external request invocation.
   --  Note: only one Object Adapter can be associated with an ORB.

   function Object_Adapter (ORB : access ORB_Type)
     return Obj_Adapters.Obj_Adapter_Access;
   --  Return the object adapter associated with ORB.

   procedure Create_Reference
     (ORB : access ORB_Type;
      Oid : access Objects.Object_Id;
      Typ : in     String;
      Ref :    out References.Ref);
   --  Create an object reference that designates object Oid
   --  within this ORB.

   function Handle_Message
     (ORB : access ORB_Type;
      Msg : PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class;

private

   --------------------------------------------
   -- Job type for method execution requests --
   --------------------------------------------

   type Request_Job is new PJ.Job with record
      ORB       : ORB_Access;
      Request   : Requests.Request_Access;
      Requestor : Components.Component_Access;
   end record;

   procedure Run (J : access Request_Job);
   --  Overload the abstract Run primitive for Job:
   --  dispatch through tasking policy.

   procedure Run_Request (J : access Request_Job);
   --  Execute the request associated with J within the
   --  current task.

   -------------------------------------
   -- Reactor for asynchronous events --
   -------------------------------------

   --  The middleware core implements the Reactor pattern
   --  to handle event occurring on asynchronous event sources.
   --  An event handler is associated with each asynchronous
   --  event source. The handling of an event constitutes
   --  a Job that can be performed by an ORB task.

   type AES_Event_Handler is abstract new PolyORB.Jobs.Job with record
      ORB : ORB_Access;
      AES : PAE.Asynch_Ev_Source_Access;
   end record;

   procedure Run (AEH : access AES_Event_Handler);
   --  Call Handle_Event.

   procedure Handle_Event
     (H   : access AES_Event_Handler;
      ORB :        ORB_Access;
      AES : in out PAE.Asynch_Ev_Source_Access)
      is abstract;
   --  Handle an event that has occurred on this asynchronous
   --  event source. If AES is null on exit, then the asynchronous
   --  event source has been destroyed, and the handler must be
   --  deallocated.

   --  In this implementation of the Reactor pattern, the
   --  association between an event source and its event
   --  handler is made using an Annotation on the event source.

   type AES_Event_Handler_Access is access AES_Event_Handler'Class;

   type AES_Note is new Annotations.Note with record
      Handler : AES_Event_Handler_Access;
   end record;

   ---------------------------------------
   -- Tasking policy abstract interface --
   ---------------------------------------

   type Tasking_Policy_Type is abstract tagged limited null record;

   package Monitor_Seqs is new PolyORB.Sequences.Unbounded
     (PAE.Asynch_Ev_Monitor_Access);
   subtype Monitor_Seq is Monitor_Seqs.Sequence;

   package TAP_Seqs is new PolyORB.Sequences.Unbounded
     (PT.Transport_Access_Point_Access);
   subtype TAP_Seq is TAP_Seqs.Sequence;

   type ORB_Type (Tasking_Policy : access Tasking_Policy_Type'Class)
   is new PolyORB.Components.Component with record

      -----------------------------------
      -- Mutex for access to ORB state --
      -----------------------------------

      ORB_Lock : PolyORB.Tasking.Advanced_Mutexes.Adv_Mutex_Access;

      ------------------
      -- Server state --
      ------------------

      Shutdown   : Boolean := False;
      --  Set to True when ORB shutdown has been requested.

      Job_Queue  : PJ.Job_Queue_Access;
      --  The queue of jobs to be processed by ORB tasks.

      Idle_Tasks : PolyORB.Tasking.Watchers.Watcher_Access;
      --  Idle ORB task wait on this watcher.

      Idle_Counter : Natural;
      --  Number of thread in the Idle State

      Monitors : Monitor_Seq;
      --  The set of asynchronous event monitors to be watched
      --  by ORB tasks.

      Transport_Access_Points : TAP_Seq;
      --  The set of transport access points managed by this ORB.

      Polling : Boolean;
      --  True if, and only if, one task is blocked waiting
      --  for external events on ORB_Sockets.

      Source_Deleted : Boolean;
      --  Signals whether Delete_Source has been called while
      --  another task was polling.

      Polling_Watcher : PolyORB.Tasking.Watchers.Watcher_Access;
      Polling_Version : PolyORB.Tasking.Watchers.Version_Id;
      --  This watcher is looked up before one task goes into
      --  external event polling, and updated after polling
      --  is completed and events have been processed.
      --  Notionally, Polling_Version is the version of the
      --  set of AESs supported by Monitors that is being
      --  considered, and while it is being considered
      --  no AES may be destroyed.

      Selector : PAE.Asynch_Ev_Monitor_Access;
      --  The asynchronous event monitor on which this ORB is
      --  currently waiting for events.
      --  XXX This is very wrong as is: there might be
      --      several ORB tasks currently blocked on different
      --      selectors. A set of task descrptors, including task
      --      origin, permanentness and current status should really
      --      be maintained.

      Obj_Adapter : Obj_Adapters.Obj_Adapter_Access;
      --  The object adapter that manages objects registered
      --  with this ORB.
   end record;

end PolyORB.ORB;
