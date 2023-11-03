------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S E R V E R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Text_IO;

with CORBA.Impl;
with CORBA.Object;
with CORBA.ORB;
with CORBA.Policy;

with PortableServer.POA.Helper;
with PortableServer.POAManager;

with RTCORBA.RTORB.Helper;
with RTCORBA.PriorityMapping.Linear;
with RTCORBA.PriorityModelPolicy;
with RTCORBA.ThreadpoolPolicy;

with RTPortableServer.POA.Helper;

with PolyORB.RTCORBA_P.Setup;

with Echo.Impl;

with PolyORB.Utils.Report;

--  Begin of PolyORB's setup

with PolyORB.ORB.Thread_Pool;
pragma Warnings (Off, PolyORB.ORB.Thread_Pool);

with PolyORB.ORB_Controller.Half_Sync_Half_Async;
pragma Warnings (Off, PolyORB.ORB_Controller.Half_Sync_Half_Async);

with PolyORB.Request_Scheduler.Servant_Lane;
pragma Warnings (Off, PolyORB.Request_Scheduler.Servant_Lane);

with PolyORB.Tasking.Profiles.Full_Tasking.Threads.Dynamic_Priorities;
pragma Warnings
  (Off, PolyORB.Tasking.Profiles.Full_Tasking.Threads.Dynamic_Priorities);

with PolyORB.Tasking.Profiles.Full_Tasking.Threads;
pragma Warnings (Off, PolyORB.Tasking.Profiles.Full_Tasking.Threads);

with PolyORB.Tasking.Profiles.Full_Tasking.Threads.Annotations;
pragma Warnings
  (Off, PolyORB.Tasking.Profiles.Full_Tasking.Threads.Annotations);

with PolyORB.Tasking.Profiles.Full_Tasking.Mutexes;
pragma Warnings (Off, PolyORB.Tasking.Profiles.Full_Tasking.Mutexes);

with PolyORB.Tasking.Profiles.Full_Tasking.Condition_Variables;
pragma Warnings
  (Off, PolyORB.Tasking.Profiles.Full_Tasking.Condition_Variables);

with PolyORB.Setup.Base;
pragma Warnings (Off, PolyORB.Setup.Base);

with PolyORB.Setup.OA.Basic_RT_POA;
pragma Warnings (Off, PolyORB.Setup.OA.Basic_RT_POA);

with PolyORB.Setup.IIOP;
pragma Warnings (Off, PolyORB.Setup.IIOP);

with PolyORB.Setup.Access_Points.IIOP;
pragma Warnings (Off, PolyORB.Setup.Access_Points.IIOP);

with PolyORB.GIOP_P.Tagged_Components.Policies.Priority_Model_Policy;
pragma Warnings
  (Off, PolyORB.GIOP_P.Tagged_Components.Policies.Priority_Model_Policy);

--  End of PolyORB's setup

procedure Server is

   use Ada.Text_IO;

   use CORBA.ORB;
   use CORBA.Policy.IDL_SEQUENCE_Policy;

   use PortableServer;
   use PortableServer.POA;

   use RTCORBA;
   use RTCORBA.RTORB;

   use PolyORB.Utils.Report;

   Priority_Mapping : RTCORBA.PriorityMapping.Linear.Object;

begin
   CORBA.ORB.Initialize ("ORB");

   --  Setting up default Priority Mapping for this node

   PolyORB.RTCORBA_P.Setup.Set_Priority_Mapping (Priority_Mapping);

   Output ("ORB is configured", True);

   New_Test ("CLIENT_PROPAGATED server");

   declare
      use RTCORBA.IDL_SEQUENCE_RTCORBA_ThreadpoolLane;

      RT_ORB : RTCORBA.RTORB.Local_Ref;

      Root_POA : PortableServer.POA.Local_Ref;

      --  Variables for Child_POA #1

      Obj_Server_1 : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;

      Priority_Model_Policy_Ref_1 : RTCORBA.PriorityModelPolicy.Local_Ref;

      Thread_Pool_Id_1 : RTCORBA.ThreadpoolId;

      Lanes : RTCORBA.ThreadpoolLanes;

      Thread_Pool_Policy_Ref_1 : RTCORBA.ThreadpoolPolicy.Local_Ref;
      Policies_1 : CORBA.Policy.PolicyList;
      Child_POA_Server_1 : RTPortableServer.POA.Local_Ref;

      Ref_Server_1 : CORBA.Object.Ref;

      Base_Priority      : constant RTCORBA.Priority := 0;
      Default_Priority_1 : constant RTCORBA.Priority := 10_000;
      Default_Priority_2 : constant RTCORBA.Priority := 20_000;

   begin

      --  Retrieve RT ORB

      RT_ORB := RTCORBA.RTORB.Helper.To_Local_Ref
        (Resolve_Initial_References
         (To_CORBA_String ("RTORB")));

      Output ("Retrieved reference on RT ORB", True);

      --  Retrieve Root POA

      Root_POA := PortableServer.POA.Helper.To_Local_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      PortableServer.POAManager.Activate
        (PortableServer.POA.Get_The_POAManager (Root_POA));

      Output ("Retrieved and activated Root POA", True);

      New_Test ("Setting up Child_POA #1");

      --  Create CLIENT_PROPAGATED PriorityModel policy

      Priority_Model_Policy_Ref_1
        := Create_Priority_Model_Policy
        (RT_ORB,
         CLIENT_PROPAGATED,
         Base_Priority);

      Output ("CLIENT_PROPAGATED policy declared", True);

      --  Create Lanes

      Append (Lanes,
              RTCORBA.ThreadpoolLane'(lane_priority => Base_Priority,
                                      static_threads => 2,
                                      dynamic_threads => 0));

      Append (Lanes,
               RTCORBA.ThreadpoolLane'(lane_priority => Default_Priority_1,
                                       static_threads => 2,
                                       dynamic_threads => 0));

      Append (Lanes,
               RTCORBA.ThreadpoolLane'(lane_priority => Default_Priority_2,
                                       static_threads => 2,
                                       dynamic_threads => 0));

      Output ("Lanes created", True);

      --  Construct Thread Pool policy from previous threadpool

      Thread_Pool_Id_1 := Create_Threadpool_With_Lanes
        (RT_ORB,
         Stacksize               => 262_144,
         Lanes                   => Lanes,
         Allow_Borrowing         => False,
         Allow_Request_Buffering => False,
         Max_Buffered_Requests   => 1,
         Max_Request_Buffer_Size => 0);

      Thread_Pool_Policy_Ref_1 := RTCORBA.RTORB.Create_Threadpool_Policy
           (RT_ORB, Thread_Pool_Id_1);
      Output ("Create Threadpool policy", True);

      --  Create Child POA with CLIENT_PROPAGATED priority model policy

      Append (Policies_1,
              CORBA.Policy.Ref (Priority_Model_Policy_Ref_1));

      Append (Policies_1,
              CORBA.Policy.Ref (Thread_Pool_Policy_Ref_1));

      Child_POA_Server_1 := RTPortableServer.POA.Helper.To_Local_Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          CORBA.To_CORBA_String ("Child_POA_Server_1"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies_1));

      Output ("Create Child POA with these policies", True);

      --  Set up new object and attach it to Child_POA

      Ref_Server_1 := PortableServer.POA.Servant_To_Reference
        (PortableServer.POA.Local_Ref (Child_POA_Server_1),
         PortableServer.Servant (Obj_Server_1));

      Output ("Implicit activation of an object with these policies", True);

      --  Output object IOR

      Put_Line ("IOR of object #1, attached to thread pool with 3 lanes "
                & "with priorities"
                & RTCORBA.Priority'Image (Base_Priority)
                & ","
                & RTCORBA.Priority'Image (Default_Priority_1)
                & ","
                & RTCORBA.Priority'Image (Default_Priority_2));

      New_Line;
      Put_Line
        ("'"
         & CORBA.To_Standard_String
         (CORBA.Object.Object_To_String (Ref_Server_1))
         & "'");
      New_Line;

      --  Launch the server

      Output ("Server is running", True);
      New_Line;

      CORBA.ORB.Run;
   end;

   End_Report;

exception
   when E : others =>
      New_Line;
      Put_Line ("Got exception: "
                & Ada.Exceptions.Exception_Information (E));

      Output ("Test failed", False);
      End_Report;
end Server;
