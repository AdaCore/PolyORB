------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . O R B                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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
with PolyORB.Binding_Data;
with PolyORB.Components;
with PolyORB.Filters;
with PolyORB.Jobs;
with PolyORB.Obj_Adapters;
with PolyORB.Objects;
with PolyORB.References;
with PolyORB.Requests;
with PolyORB.Sequences.Unbounded;
with PolyORB.Soft_Links;
with PolyORB.Task_Info;
with PolyORB.Transport;

package PolyORB.ORB is

   use PolyORB.Asynch_Ev;
   use PolyORB.Transport;
   use PolyORB.Components;

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

   procedure Run_And_Free_Job
     (P : access Tasking_Policy_Type;
      J : in out Jobs.Job_Access);
   --  Execute job J in the context of tasking policy P.
   --  J is ran in the context of the calling task, except in
   --  cases where the policy mandates otherwise.
   --  J is freed afterwards.

   function Duplicate_Request_Job
     (RJ : access Jobs.Job'Class)
     return Jobs.Job_Access;
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

   procedure Handle_Request_Execution
     (P   : access Tasking_Policy_Type;
      ORB : ORB_Access;
      RJ  : access Jobs.Job'Class)
      is abstract;
   --  Create the necessary processing resources for the execution
   --  of request execution job RJ, and start this execution.
   --  RJ is freed after it is called.

   procedure Idle
     (P   : access Tasking_Policy_Type;
      ORB : ORB_Access) is abstract;
   --  Called by a task that has nothing to do in order
   --  to wait until there may be anything to do.

   procedure Queue_Request_To_Handler
     (P   : access Tasking_Policy_Type;
      ORB : ORB_Access;
      Msg : Message'Class) is abstract;

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
      TAP   : Transport_Access_Point_Access;
      Chain : Filters.Factory_Access;
      PF    : Binding_Data.Profile_Factory_Access);
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
      TE           :        Transport_Endpoint_Access;
      Filter_Stack :        Filters.Filter_Access;
      Role         :        Endpoint_Role);
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

   type Tasking_Policy_Type is abstract tagged limited null record;

   package Monitor_Seqs is new PolyORB.Sequences.Unbounded
     (Asynch_Ev.Asynch_Ev_Monitor_Access);
   subtype Monitor_Seq is Monitor_Seqs.Sequence;

   package TAP_Seqs is new PolyORB.Sequences.Unbounded
     (Transport.Transport_Access_Point_Access);
   subtype TAP_Seq is TAP_Seqs.Sequence;

   type ORB_Type (Tasking_Policy : access Tasking_Policy_Type'Class)
   is new PolyORB.Components.Component with record

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

      Transport_Access_Points : TAP_Seq;
      --  The set of transport access points managed by this ORB.

      Polling : Boolean;
      --  True if, and only if, one task is blocked waiting
      --  for external events on ORB_Sockets.

      Selector : Asynch_Ev.Asynch_Ev_Monitor_Access;
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

   -------------------------------------
   -- Internal primitives of ORB_Type --
   -------------------------------------

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

   --------------------------------------------
   -- Job type for method execution requests --
   --------------------------------------------

   type Request_Job is new Jobs.Job with record
      ORB       : ORB_Access;
      Request   : Requests.Request_Access;
      Requestor : Components.Component_Access;
   end record;

   procedure Run (J : access Request_Job);

end PolyORB.ORB;
