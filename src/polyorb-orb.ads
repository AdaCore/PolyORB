------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . O R B                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
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
with PolyORB.Scheduler;
with PolyORB.Tasking.Mutexes;
with PolyORB.Task_Info;
with PolyORB.Transport;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;

package PolyORB.ORB is

   package PAE  renames PolyORB.Asynch_Ev;
   package PBD  renames PolyORB.Binding_Data;
   package PC   renames PolyORB.Components;
   package PF   renames PolyORB.Filters;
   package PJ   renames PolyORB.Jobs;
   package PS   renames PolyORB.Scheduler;
   package PT   renames PolyORB.Transport;
   package PTM  renames PolyORB.Tasking.Mutexes;

   type Request_Job is new PJ.Job with private;

   ----------------------------------
   -- Abstract tasking policy type --
   ----------------------------------

   --  A tasking policy defines a set of associations between the
   --  reception of certain messages or the detection of events on the
   --  ORB_Type component defined above and the resources used to
   --  process them. Each association is embodied in a specific
   --  subprogram. This subprogram may do all kinds of actions to
   --  handle the message: job, task creation or schedule it for
   --  execution by a general-purpose ORB task.

   type Tasking_Policy_Type is abstract tagged limited private;

   type Tasking_Policy_Access is access all Tasking_Policy_Type'Class;

   ---------------------
   -- A server object --
   ---------------------

   --  XXX this is not a server object !!!

   type ORB_Type (Tasking_Policy : access Tasking_Policy_Type'Class)
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
      ORB :        ORB_Access;
      C   :        Active_Connection)
      is abstract;
   --  Create the necessary processing resources for newly-created
   --  communication endpoint AS on server side.

   procedure Handle_Close_Server_Connection
     (P   : access Tasking_Policy_Type;
      TE  :        PT.Transport_Endpoint_Access)
      is abstract;
   --  Do necessary processing when a connection is closed

   procedure Handle_New_Client_Connection
     (P   : access Tasking_Policy_Type;
      ORB :        ORB_Access;
      C   :        Active_Connection)
      is abstract;
   --  Create the necessary processing resources for newly-created
   --  communication endpoint AS on client side.

   procedure Handle_Request_Execution
     (P   : access Tasking_Policy_Type;
      ORB :        ORB_Access;
      RJ  : access Request_Job'Class)
      is abstract;
   --  Create the necessary processing resources for the execution
   --  of request execution job RJ, and start this execution.
   --  RJ is freed after it is called.

   procedure Idle
     (P         : access Tasking_Policy_Type;
      This_Task :        PolyORB.Task_Info.Task_Info;
      ORB       :        ORB_Access)
      is abstract;
   --  Called by a task that has nothing to do in order to
   --  wait until there may be anything to do.
   --  The calling task must hold ORB_Lock before calling Idle;
   --  the tasking policy shall release it while the task is
   --  idling, and re-assert it before Idle returns.
   --  This_Task holds information on this waiting task.

   procedure Queue_Request_To_Handler
     (P   : access Tasking_Policy_Type;
      ORB :        ORB_Access;
      Msg :        PolyORB.Components.Message'Class)
      is abstract;
   --  Assign the handling of a Request (i.e. an upcall to
   --  an application object) to the appropriate task.
   --  XXX It looks like this is implemented in exactly identical terms
   --  by all existing ORB tasking policies (as of 20021212). This should
   --  be removed from the TP interface.

   ------------------------------
   -- Server object operations --
   ------------------------------

   type Task_Info_Access_Access is
     access all PolyORB.Task_Info.Task_Info_Access;

   type Exit_Condition_T is record
      Condition : PolyORB.Types.Boolean_Ptr;
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
   --  Perform one ORB job and return.

   procedure Shutdown
     (ORB                 : access ORB_Type;
      Wait_For_Completion :        Boolean := True);
   --  Shutdown ORB. If Wait_For_Completion is True, do
   --  not return before the shutdown is completed.

   procedure Register_Access_Point
     (ORB   : access ORB_Type;
      TAP   :        PT.Transport_Access_Point_Access;
      Chain :        PF.Factory_Access;
      PF    :        PBD.Profile_Factory_Access);
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
      OA  :        Obj_Adapters.Obj_Adapter_Access);
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
      Msg :        PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class;

   ----------------------------------------
   -- Utility routines for jobs handling --
   ----------------------------------------

   function Duplicate_Request_Job
     (RJ : access PJ.Job'Class)
     return PJ.Job_Access;
   --  Create a copy of RJ, a Request_Job, so it can be stored
   --  for later execution.

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

   ---------------------------------------
   -- Tasking policy abstract interface --
   ---------------------------------------

   type Tasking_Policy_Type is abstract tagged limited null record;

   package Monitor_Lists is new PolyORB.Utils.Chained_Lists
     (PAE.Asynch_Ev_Monitor_Access, PAE."=");
   subtype Monitor_List is Monitor_Lists.List;

   package TAP_Lists is new PolyORB.Utils.Chained_Lists
     (PT.Transport_Access_Point_Access, PT."=");
   subtype TAP_List is TAP_Lists.List;

   ---------------------
   -- A server object --
   ---------------------

   type ORB_Type (Tasking_Policy : access Tasking_Policy_Type'Class)
   is new PolyORB.Components.Component with record

      -----------------------------------
      -- Mutex for access to ORB state --
      -----------------------------------

      ORB_Lock : PTM.Mutex_Access;

      -----------------------
      -- Scheduling Policy --
      -----------------------

      Scheduling_Policy : PS.Scheduling_Policy_Access;

      ------------------
      -- Server state --
      ------------------

      Job_Queue  : PJ.Job_Queue_Access;
      --  The queue of jobs to be processed by ORB tasks.

      Monitors : Monitor_List;
      --  The set of asynchronous event monitors to be watched
      --  by ORB tasks.

      Number_Of_Monitors : Natural := 0;
      --  Length of list 'Monitors'. This value is precomputed as it
      --  is written seldom, read often.

      Transport_Access_Points : TAP_List;
      --  The set of transport access points managed by this ORB.

      Polling : Boolean;
      --  True if, and only if, one task is blocked waiting
      --  for external events on an Asynchronous Event Monitor.
      --  XXX This flag is for debug purpose only, keep it for a while
      --  to test implementation consistency.

      Obj_Adapter : Obj_Adapters.Obj_Adapter_Access;
      --  The object adapter that manages objects registered
      --  with this ORB.
   end record;

end PolyORB.ORB;
