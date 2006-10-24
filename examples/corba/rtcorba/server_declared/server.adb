------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S E R V E R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

   New_Test ("SERVER_DECLARED server");

   declare
      RT_ORB : RTCORBA.RTORB.Local_Ref;

      Root_POA : PortableServer.POA.Local_Ref;

      --  Variables for Child_POA #1

      Obj_Server_1 : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;

      Priority_Model_Policy_Ref_1 : RTCORBA.PriorityModelPolicy.Local_Ref;
      Thread_Pool_Id_1 : RTCORBA.ThreadpoolId;
      Thread_Pool_Policy_Ref_1 : RTCORBA.ThreadpoolPolicy.Local_Ref;
      Policies_1 : CORBA.Policy.PolicyList;
      Child_POA_Server_1 : RTPortableServer.POA.Local_Ref;

      Ref_Server_1 : CORBA.Object.Ref;

      Default_Priority_1 : constant RTCORBA.Priority := 10_000;

      --  Variables for Child_POA #2

      Obj_Server_2 : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;

      Priority_Model_Policy_Ref_2 : RTCORBA.PriorityModelPolicy.Local_Ref;
      Thread_Pool_Id_2 : RTCORBA.ThreadpoolId;
      Thread_Pool_Policy_Ref_2 : RTCORBA.ThreadpoolPolicy.Local_Ref;
      Policies_2 : CORBA.Policy.PolicyList;
      Child_POA_Server_2 : RTPortableServer.POA.Local_Ref;

      Ref_Server_2 : CORBA.Object.Ref;

      Default_Priority_2 : constant RTCORBA.Priority := 20_000;

      --  Variables for Child_POA #3

      Obj_Server_3 : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;

      Lanes : RTCORBA.ThreadpoolLanes;
      Priority_Model_Policy_Ref_3 : RTCORBA.PriorityModelPolicy.Local_Ref;
      Thread_Pool_Id_3 : RTCORBA.ThreadpoolId;
      Thread_Pool_Policy_Ref_3 : RTCORBA.ThreadpoolPolicy.Local_Ref;
      Policies_3 : CORBA.Policy.PolicyList;
      Child_POA_Server_3 : RTPortableServer.POA.Local_Ref;

      Ref_Server_3 : CORBA.Object.Ref;

      No_Implicit_Activation_Policy : CORBA.Policy.Ref
        := CORBA.Policy.Ref
        (Create_Implicit_Activation_Policy (NO_IMPLICIT_ACTIVATION));

   begin

      --  Retrieve RT ORB

      RT_ORB := RTCORBA.RTORB.Helper.To_Local_Ref
        (Resolve_Initial_References
         (To_CORBA_String ("RTORB")));

      Output ("Retrieved reference on RT ORB", True);

      --  Retrieve Root POA

      Root_POA := PortableServer.POA.Helper.To_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      PortableServer.POAManager.Activate
        (PortableServer.POA.Get_The_POAManager (Root_POA));

      Output ("Retrieved and activated Root POA", True);

      --  Test #1

      New_Test ("Setting up Child_POA #1");

      --  Create SERVER_DECLARED PriorityModel policy

      Priority_Model_Policy_Ref_1
        := Create_Priority_Model_Policy
        (RT_ORB,
         SERVER_DECLARED,
         Default_Priority_1);

      Output ("SERVER_DECLARED policy declared", True);

      --  Create Threadpool

      Thread_Pool_Id_1 := RTCORBA.RTORB.Create_Threadpool
        (RT_ORB,
         Stacksize               => 262_144,
         Static_Threads          => 2,
         Dynamic_Threads         => 0,
         Default_Priority        => Default_Priority_1,
         Allow_Request_Buffering => False,
         Max_Buffered_Requests   => 1,
         Max_Request_Buffer_Size => 0);

      Output ("Thread Pool created with id"
              & RTCORBA.ThreadpoolId'Image (Thread_Pool_Id_1), True);

      --  Construct Thread Pool policy from previous threadpool

      Thread_Pool_Policy_Ref_1 := RTCORBA.RTORB.Create_Threadpool_Policy
           (RT_ORB, Thread_Pool_Id_1);
      Output ("Create Threadpool policy", True);

      --  Create Child POA with SERVER_DECLARED priority model policy

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

      Echo.Impl.Object (Obj_Server_1.all).Priority := Default_Priority_1;

      Ref_Server_1 := PortableServer.POA.Servant_To_Reference
        (PortableServer.POA.Local_Ref (Child_POA_Server_1),
         PortableServer.Servant (Obj_Server_1));

      Output ("Implicit activation of an object with these policies", True);

      --  Test #2

      New_Test ("Setting up Child_POA #2");

      --  Create SERVER_DECLARED PriorityModel policy

      Priority_Model_Policy_Ref_2
        := Create_Priority_Model_Policy
        (RT_ORB,
         SERVER_DECLARED,
         Default_Priority_2);

      Output ("SERVER_DECLARED policy declared", True);

      --  Create Threadpool

      Thread_Pool_Id_2 := RTCORBA.RTORB.Create_Threadpool
        (RT_ORB,
         Stacksize               => 262_144,
         Static_Threads          => 2,
         Dynamic_Threads         => 0,
         Default_Priority        => Default_Priority_2,
         Allow_Request_Buffering => True,
         Max_Buffered_Requests   => 1,
         Max_Request_Buffer_Size => 0);

      Output ("Thread Pool created with id"
              & RTCORBA.ThreadpoolId'Image (Thread_Pool_Id_2), True);

      --  Construct Thread Pool policy from previous threadpool

      Thread_Pool_Policy_Ref_2 := RTCORBA.RTORB.Create_Threadpool_Policy
           (RT_ORB, Thread_Pool_Id_2);
      Output ("Create Threadpool policy", True);

      --  Create Child POA with SERVER_DECLARED priority model policy

      Append (Policies_2,
              CORBA.Policy.Ref (Priority_Model_Policy_Ref_2));

      Append (Policies_2,
              CORBA.Policy.Ref (Thread_Pool_Policy_Ref_2));

      Child_POA_Server_2 := RTPortableServer.POA.Helper.To_Local_Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          CORBA.To_CORBA_String ("Child_POA_Server_2"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies_2));

      Output ("Create Child POA with these policies", True);

      --  Set up new object and attach it to Child_POA

      Echo.Impl.Object (Obj_Server_2.all).Priority := Default_Priority_2;

      Ref_Server_2 := PortableServer.POA.Servant_To_Reference
        (PortableServer.POA.Local_Ref (Child_POA_Server_2),
         PortableServer.Servant (Obj_Server_2));

      Output ("Implicit activation of an object with these policies", True);

      --  Test #3

      New_Test ("Setting up Child_POA #3");

      --  Create SERVER_DECLARED PriorityModel policy

      Priority_Model_Policy_Ref_3
        := Create_Priority_Model_Policy
        (RT_ORB,
         SERVER_DECLARED,
         Default_Priority_2);

      Output ("SERVER_DECLARED policy declared", True);

      --  Create Lanes

      Append (Lanes,
               RTCORBA.ThreadpoolLane'(Lane_Priority => Default_Priority_1,
                                       Static_Threads => 2,
                                       Dynamic_Threads => 0));

      Append (Lanes,
               RTCORBA.ThreadpoolLane'(Lane_Priority => Default_Priority_2,
                                       Static_Threads => 2,
                                       Dynamic_Threads => 0));

      --  Create Threadpool

      Thread_Pool_Id_3 := RTCORBA.RTORB.Create_Threadpool_With_Lanes
        (RT_ORB,
         Stacksize               => 262_144,
         Lanes                   => Lanes,
         Allow_Borrowing         => False,
         Allow_Request_Buffering => False,
         Max_Buffered_Requests   => 1,
         Max_Request_Buffer_Size => 0);

      Output ("Thread Pool created with id"
              & RTCORBA.ThreadpoolId'Image (Thread_Pool_Id_3), True);

      --  Construct Thread Pool policy from previous threadpool

      Thread_Pool_Policy_Ref_3 := RTCORBA.RTORB.Create_Threadpool_Policy
           (RT_ORB, Thread_Pool_Id_3);
      Output ("Create Threadpool policy", True);

      --  Create Child POA with SERVER_DECLARED priority model policy

      Append (Policies_3,
              CORBA.Policy.Ref (Priority_Model_Policy_Ref_3));

      Append (Policies_3,
              CORBA.Policy.Ref (Thread_Pool_Policy_Ref_3));

      Append (Policies_3, No_Implicit_Activation_Policy);

      Child_POA_Server_3 := RTPortableServer.POA.Helper.To_Local_Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          CORBA.To_CORBA_String ("Child_POA_Server_3"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies_3));

      Output ("Create Child POA with these policies", True);

      --  Set up new object and attach it to Child_POA

      Echo.Impl.Object (Obj_Server_3.all).Priority := Default_Priority_2;

      declare
         Oid : constant PortableServer.ObjectId
           := RTPortableServer.POA.Activate_Object_With_Priority
           (Child_POA_Server_3,
            PortableServer.Servant (Obj_Server_3),
            Default_Priority_2);

      begin
         Output ("Activate_Object_With_Priority did not raise exception",
                 True);

         --  Call Servant_To_Reference

         Ref_Server_3 := PortableServer.POA.Id_To_Reference
           (PortableServer.POA.Local_Ref (Child_POA_Server_3), Oid);

         --  Building array of objects for client processing

         Echo.Impl.Echo_Objects (0) := Ref_Server_1;
         Echo.Impl.Echo_Objects (1) := Ref_Server_2;
         Echo.Impl.Echo_Objects (2) := Ref_Server_3;

         --  Output object IORs

         Put_Line ("IOR of object #1, at RTCORBA priority"
                   & RTCORBA.Priority'Image (Default_Priority_1));

         New_Line;
         Put_Line
           ("'"
            & CORBA.To_Standard_String
            (CORBA.Object.Object_To_String (Ref_Server_1))
            & "'");
         New_Line;

         Put_Line ("IOR of object #2, at RTCORBA priority"
                   & RTCORBA.Priority'Image (Default_Priority_2));

         New_Line;
         Put_Line
           ("'"
            & CORBA.To_Standard_String
            (CORBA.Object.Object_To_String (Ref_Server_2))
            & "'");
         New_Line;

         Put_Line ("IOR of object #3, at RTCORBA priority"
                   & RTCORBA.Priority'Image (Default_Priority_2));

         New_Line;
         Put_Line
           ("'"
            & CORBA.To_Standard_String
            (CORBA.Object.Object_To_String (Ref_Server_3))
            & "'");
         New_Line;

         --  Run ORB

         Output ("Server is running", True);
         New_Line;

         CORBA.ORB.Run;
      end;
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
