------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . O R B                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2009, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  The ORB core module: main loop and scheduler.
--  Role: * to coordinate operation of the various subsystems.
--        * to gateway asynchronous external events to the
--          synchronous messaging architecture used within PolyORB.

with PolyORB.Annotations;
with PolyORB.Asynch_Ev;
with PolyORB.Binding_Data;
with PolyORB.Binding_Objects;
with PolyORB.Binding_Objects.Lists;
with PolyORB.Components;
with PolyORB.Filters;
with PolyORB.Jobs;
with PolyORB.ORB_Controller;
with PolyORB.Obj_Adapters;
with PolyORB.Objects;
with PolyORB.QoS;
with PolyORB.References;
with PolyORB.Requests;
with PolyORB.Smart_Pointers;
with PolyORB.Task_Info;
with PolyORB.Transport;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;

package PolyORB.ORB is

   package PAE  renames PolyORB.Asynch_Ev;
   package PBD  renames PolyORB.Binding_Data;
   package PBO  renames PolyORB.Binding_Objects;
   package PBOL renames PolyORB.Binding_Objects.Lists;
   package PC   renames PolyORB.Components;
   package PF   renames PolyORB.Filters;
   package PJ   renames PolyORB.Jobs;
   package POC  renames PolyORB.ORB_Controller;
   package PT   renames PolyORB.Transport;
   package PTI  renames PolyORB.Task_Info;

   ----------------------------------
   -- Abstract tasking policy type --
   ----------------------------------

   --  A tasking policy defines a set of associations between the reception of
   --  certain messages or the detection of events on the ORB_Type component
   --  defined above and the resources used to process them. Each association
   --  is embodied in a specific subprogram. This subprogram may do all kinds
   --  of actions to handle the message: job, task creation or schedule it for
   --  execution by a general-purpose ORB task.

   type Tasking_Policy_Type is abstract tagged limited private;
   type Tasking_Policy_Access is access all Tasking_Policy_Type'Class;

   ---------------------
   -- A server object --
   ---------------------

   --  XXX this is not a server object !!!

   type ORB_Type (Tasking_Policy : access Tasking_Policy_Type'Class;
                  ORB_Controller :        POC.ORB_Controller_Access)
   is new PolyORB.Components.Component with private;

   type ORB_Access is access all ORB_Type;

   -----------------
   -- Request_Job --
   -----------------

   type Request_Job is new PJ.Job with record
      ORB       : ORB_Access;
      Request   : Requests.Request_Access;
   end record;

   -------------------------------
   -- Tasking policy operations --
   -------------------------------

   type Active_Connection is record
      AES : PAE.Asynch_Ev_Source_Access;
      TE  : PT.Transport_Endpoint_Access;
   end record;

   procedure Handle_New_Server_Connection
     (P   : access Tasking_Policy_Type;
      ORB :        ORB_Access;
      AC  :        Active_Connection) is abstract;
   --  Create the necessary processing resources for newly-created
   --  communication endpoint AS on server side.

   procedure Handle_Close_Connection
     (P   : access Tasking_Policy_Type;
      TE  : PT.Transport_Endpoint_Access) is abstract;
   --  Do necessary processing when a connection is closed

   procedure Handle_New_Client_Connection
     (P   : access Tasking_Policy_Type;
      ORB : ORB_Access;
      AC  : Active_Connection) is abstract;
   --  Create the necessary processing resources for newly-created
   --  communication endpoint AS on client side.

   procedure Handle_Request_Execution
     (P   : access Tasking_Policy_Type;
      ORB : ORB_Access;
      RJ  : access Request_Job'Class) is abstract;
   --  Create the necessary processing resources for the execution of request
   --  execution job RJ, which must be an upcall, and start this execution.
   --  RJ is freed automatically after completion.

   procedure Idle
     (P         : access Tasking_Policy_Type;
      This_Task : in out PTI.Task_Info;
      ORB       : ORB_Access) is abstract;
   --  Called by a task that has nothing to do.
   --  The calling task must be in the ORB critical section at the call point;
   --  the tasking policy shall release it while the task is idling, and
   --  re-assert it before Idle returns. This_Task holds information on the
   --  idling task.

   ------------------------------
   -- Server object operations --
   ------------------------------

   type Task_Info_Access_Access is access all PTI.Task_Info_Access;

   type Exit_Condition_T is record
      Condition : PolyORB.Types.Boolean_Ptr;
      Task_Info : Task_Info_Access_Access;
   end record;

   procedure Create (ORB : in out ORB_Type);
   --  Initialize a newly-allocated ORB object

   procedure Queue_Request_To_Handler
     (ORB : access ORB_Type;
      Msg : PolyORB.Components.Message'Class);
   --  Assign the handling of a Request (i.e. an upcall to an application
   --  object) to the appropriate task.
   --  ??? This is also used on the client side in Invoke_Request to submit
   --  a request to the ORB.

   function Find_Reusable_Binding_Object
     (ORB : access ORB_Type;
      Pro : Binding_Data.Profile_Access;
      QoS : PolyORB.QoS.QoS_Parameters) return Smart_Pointers.Ref;
   --  Try to find a binding object with a profile compatible with Pro, to
   --  determine if it can be reused for binding Pro. Return a reference to a
   --  Binding Object if found, or a nil reference if not.

   procedure Run
     (ORB            : access ORB_Type;
      Exit_Condition : Exit_Condition_T := (null, null);
      May_Exit       : Boolean);
   --  Execute the ORB until:
   --    - Exit_Condition.Condition.all becomes True
   --      (if Exit_Condition.Condition /= null), or
   --    - Shutdown is called on this ORB.

   --  This procedure is executed by permanent ORB tasks (those with
   --  Exit_Condition.Condition = null), and is also entered by user tasks that
   --  need to wait for a certain condition to occur.

   --  If Exit_Condition.Task_Info is not null, it is set on entry into Run to
   --  an access value that designates this task's Task_Info structure while it
   --  is executing ORB.Run.

   --  For a permanent task, if May_Exit is False then the task remains in this
   --  procedure until ORB shutdown, else it may return earlier (in which case
   --  it is expected to complete).

   --  For a transient task, May_Exit has no effect and is expected to always
   --  be set True.

   function Work_Pending (ORB : access ORB_Type) return Boolean;
   --  Return True if, and only if, some ORB processing is
   --  pending.

   procedure Perform_Work (ORB : access ORB_Type);
   --  Perform one ORB job and return.

   procedure Shutdown
     (ORB                 : access ORB_Type;
      Wait_For_Completion : Boolean := True);
   --  Shutdown ORB. If Wait_For_Completion is True, do not return before the
   --  shutdown is completed.

   procedure Register_Access_Point
     (ORB   : access ORB_Type;
      TAP   :        PT.Transport_Access_Point_Access;
      Chain :        PF.Factories_Access;
      PF    :        PBD.Profile_Factory_Access);
   --  Register a newly-created transport access point with ORB. When a
   --  connection is received on TAP, a filter chain is instantiated using
   --  Chain, and associated to the corresponding transport endpoint.

   function Is_Profile_Local
     (ORB : access ORB_Type;
      P   : access Binding_Data.Profile_Type'Class)
     return Boolean;
   --  True iff P designates an object managed by this ORB.

   type Endpoint_Role is (Client, Server);

   procedure Register_Binding_Object
     (ORB  : access ORB_Type;
      BO   :        Smart_Pointers.Ref;
      Role :        Endpoint_Role);
   --  Register a newly-created transport endpoint with ORB.
   --  A filter chain is instantiated using Chain, and associated with TE.

   procedure Unregister_Binding_Object
     (ORB : access ORB_Type;
      BO  : Binding_Objects.Binding_Object_Access);
   --  Unregister a Binding Object from the ORB

   package BO_Ref_Lists is
     new PolyORB.Utils.Chained_Lists (Smart_Pointers.Ref, Smart_Pointers."=");
   subtype BO_Ref_List is BO_Ref_Lists.List;
   --  A list of References to Binding Objects

   function Get_Binding_Objects (ORB : access ORB_Type) return BO_Ref_List;
   --  Return a list of references to the BOs owned by this ORB

   procedure Set_Object_Adapter
     (ORB : access ORB_Type;
      OA  :        Obj_Adapters.Obj_Adapter_Access);
   --  Associate object adapter (OA) with ORB.
   --  Objects registered with OA become visible through ORB for external
   --  request invocation.
   --  Note: only one Object Adapter can be associated with an ORB.

   function Object_Adapter (ORB : access ORB_Type)
     return Obj_Adapters.Obj_Adapter_Access;
   --  Return the object adapter associated with ORB

   procedure Create_Reference
     (ORB : access ORB_Type;
      Oid : access Objects.Object_Id;
      Typ : String;
      Ref : out References.Ref);
   --  Create an object reference that designates object Oid within this ORB

   function Handle_Message
     (ORB : access ORB_Type;
      Msg : Components.Message'Class) return Components.Message'Class;

   ----------------------------
   -- Annotations management --
   ----------------------------

   function Notepad_Of
     (ORB : access ORB_Type)
     return Annotations.Notepad_Access;

private

   --------------------------------------------
   -- Job type for method execution requests --
   --------------------------------------------

   procedure Run (J : not null access Request_Job);
   --  Override the abstract Run primitive for Job:
   --  dispatch through ORB's tasking policy.

   procedure Run_Request
     (ORB : access ORB_Type; Req : Requests.Request_Access);
   --  Execute Req within the current task of ORB. The ORB is responsible for
   --  the destruction of the request after execution.

   ---------------------------------------
   -- Tasking policy abstract interface --
   ---------------------------------------

   type Tasking_Policy_Type is abstract tagged limited null record;

   package TAP_Lists is new PolyORB.Utils.Chained_Lists
     (PT.Transport_Access_Point_Access, PT."=");
   subtype TAP_List is TAP_Lists.List;

   ---------------------
   -- A server object --
   ---------------------

   type ORB_Type (Tasking_Policy : access Tasking_Policy_Type'Class;
                  ORB_Controller :        POC.ORB_Controller_Access)
   is new PolyORB.Components.Component with record
      Transport_Access_Points : TAP_List;
      --  The set of transport access points managed by this ORB

      Binding_Objects : PBOL.List;
      --  The set of binding objects managed by this ORB

      Obj_Adapter : Obj_Adapters.Obj_Adapter_Access;
      --  The object adapter that manages objects registered with this ORB

      Notepad : aliased Annotations.Notepad;
      --  ORB's notepad. The user must ensure there is no race condition when
      --  accessing it.
   end record;

end PolyORB.ORB;
