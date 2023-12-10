------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2023, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
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

with PolyORB.ORB.Thread_Per_Session;
pragma Warnings (Off, PolyORB.ORB.Thread_Per_Session);

with PolyORB.ORB_Controller.Workers;
pragma Warnings (Off, PolyORB.ORB_Controller.Workers);

with PolyORB.Request_Scheduler.Servant_Lane;
pragma Warnings (Off, PolyORB.Request_Scheduler.Servant_Lane);

with PolyORB.Setup.Tasking.Full_Tasking;
pragma Warnings (Off, PolyORB.Setup.Tasking.Full_Tasking);

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

procedure Test000 is

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

   New_Test ("RTORB");

   declare
      RT_ORB : RTCORBA.RTORB.Local_Ref;

      Root_POA : PortableServer.POA.Local_Ref;

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

      --  Test simple Thread Pool

      New_Test ("Thread_Pool #1");

      declare
         Thread_Pool_Id : RTCORBA.ThreadpoolId;
         Thread_Pool_Policy : RTCORBA.ThreadpoolPolicy.Local_Ref;
         --  pragma Unreferenced (Thread_Pool_Policy);
         pragma Warnings (Off, Thread_Pool_Policy); --  WAG:5.02 DB08-008
         --  Assigned but never read

      begin
         Thread_Pool_Id := RTCORBA.RTORB.Create_Threadpool
           (RT_ORB,
            Stacksize               => 262_144,
            Static_Threads          => 2,
            Dynamic_Threads         => 0,
            Default_Priority        => 10,
            Allow_Request_Buffering => False,
            Max_Buffered_Requests   => 1,
            Max_Request_Buffer_Size => 0);

         Output ("Thread Pool created with id"
                 & RTCORBA.ThreadpoolId'Image (Thread_Pool_Id), True);

         --  Construct Thread Pool policy from previous threadpool

         Thread_Pool_Policy := RTCORBA.RTORB.Create_Threadpool_Policy
           (RT_ORB, Thread_Pool_Id);
         Output ("Create threadpool policy from valid ThreadpoolId", True);

         Destroy_Threadpool (RT_ORB, Thread_Pool_Id);
         Output ("Destroy threadpool", True);

         --  Construct Thread Pool policy from invalid threadpool

         declare
            Thread_Pool_2 : RTCORBA.ThreadpoolPolicy.Local_Ref;
            --  pragma Unreferenced (Thread_Pool_2);
            pragma Warnings (Off, Thread_Pool_2); --  WAG:5.02 DB08-008
            --  Assigned but never read

         begin
            Thread_Pool_2 := RTCORBA.RTORB.Create_Threadpool_Policy
              (RT_ORB, Thread_Pool_Id + 1);
            Output ("Create threadpool policy from invalid ThreadpoolId",
                    False);
         exception
            when RTCORBA.RTORB.InvalidThreadpool =>
               Output ("Create threadpool policy from invalid ThreadpoolId",
                       True);
         end;
      end;

      --  Test simple Thread Pool with lanes

      New_Test ("Thread_Pool #2");

      declare
         use IDL_SEQUENCE_RTCORBA_ThreadpoolLane;

         Lane1 : constant ThreadpoolLane
           := ThreadpoolLane'(lane_priority   => 3,
                              static_threads  => 2,
                              dynamic_threads => 0);
         Lane2 : constant ThreadpoolLane
           := ThreadpoolLane'(lane_priority   => 4,
                              static_threads  => 4,
                              dynamic_threads => 0);

         Lanes : ThreadpoolLanes;

         Thread_Pool_Id : RTCORBA.ThreadpoolId;
         Thread_Pool_Policy : RTCORBA.ThreadpoolPolicy.Local_Ref;
         --  pragma Unreferenced (Thread_Pool_Policy);
         pragma Warnings (Off, Thread_Pool_Policy); --  WAG:5.02 DB08-008
         --  Assigned but never read

      begin
         Append (Lanes, Lane1);
         Append (Lanes, Lane2);

         Thread_Pool_Id := Create_Threadpool_With_Lanes
           (RT_ORB,
            Stacksize               => 262_144,
            Lanes                   => Lanes,
            Allow_Borrowing         => False,
            Allow_Request_Buffering => False,
            Max_Buffered_Requests   => 1,
            Max_Request_Buffer_Size => 0);

         Output ("Thread Pool created with id"
                 & RTCORBA.ThreadpoolId'Image (Thread_Pool_Id), True);

         Thread_Pool_Policy := RTCORBA.RTORB.Create_Threadpool_Policy
           (RT_ORB, Thread_Pool_Id);
         Output ("Create threadpool with lanes", True);

         Destroy_Threadpool (RT_ORB, Thread_Pool_Id);
         Output ("Destroy threadpool with lanes", True);

      end;

      New_Test ("SERVER_DECLARED POA #1");

      --  Set up SERVER_DECLARED priority model policy

      declare
         Obj_Server : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;

         Priority_Model_Policy_Ref : RTCORBA.PriorityModelPolicy.Local_Ref;
         Thread_Pool_Id : RTCORBA.ThreadpoolId;
         Thread_Pool_Policy_Ref : RTCORBA.ThreadpoolPolicy.Local_Ref;
         Policies : CORBA.Policy.PolicyList;
         Child_POA_Server : RTPortableServer.POA.Local_Ref;

         Ref_Server : CORBA.Object.Ref;

      begin
         --  Create SERVER_DECLARED PriorityModel policy

         Priority_Model_Policy_Ref
           := Create_Priority_Model_Policy
           (RT_ORB,
            SERVER_DECLARED,
            10000);

         Output ("SERVER_DECLARED policy declared", True);

         --  Create Threadpool

         Thread_Pool_Id := RTCORBA.RTORB.Create_Threadpool
           (RT_ORB,
            Stacksize               => 262_144,
            Static_Threads          => 2,
            Dynamic_Threads         => 0,
            Default_Priority        => 10,
            Allow_Request_Buffering => False,
            Max_Buffered_Requests   => 1,
            Max_Request_Buffer_Size => 0);

         Output ("Thread Pool created with id"
                 & RTCORBA.ThreadpoolId'Image (Thread_Pool_Id), True);

         --  Construct Thread Pool policy from previous threadpool

         Thread_Pool_Policy_Ref := RTCORBA.RTORB.Create_Threadpool_Policy
           (RT_ORB, Thread_Pool_Id);
         Output ("Create Threadpool policy", True);

         --  Create Child POA with SERVER_DECLARED priority model policy

         Append (Policies,
                 CORBA.Policy.Ref (Priority_Model_Policy_Ref));

         Append (Policies,
                 CORBA.Policy.Ref (Thread_Pool_Policy_Ref));

         Child_POA_Server := RTPortableServer.POA.Helper.To_Local_Ref
           (PortableServer.POA.Create_POA
            (Root_POA,
             CORBA.To_CORBA_String ("Child_POA_Server"),
             PortableServer.POA.Get_The_POAManager (Root_POA),
             Policies));

         Output ("Create Child POA with these policies", True);

         --  Set up new object and attach it to Child_POA

         Ref_Server := PortableServer.POA.Servant_To_Reference
           (PortableServer.POA.Local_Ref (Child_POA_Server),
            PortableServer.Servant (Obj_Server));

         Output ("Implicit activation of an object with these policies", True);

         --  Output object IOR

         New_Line;
         Put_Line
           ("'"
            & CORBA.To_Standard_String
            (CORBA.Object.Object_To_String (Ref_Server))
            & "'");
         New_Line;

         Destroy_Threadpool (RT_ORB, Thread_Pool_Id);
         Output ("Destroy threadpool", True);

         Destroy (PortableServer.POA.Local_Ref (Child_POA_Server),
                  False, False);
         Output ("Destroy Child_POA", True);
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
end Test000;
