------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S E R V E R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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

with Ada.Exceptions;
with Ada.Text_IO;

with CORBA.Impl;
with CORBA.Object;
with CORBA.ORB;
with CORBA.Policy;

with PortableServer.POA;
with PortableServer.POAManager;

with RTCORBA.RTORB;
with RTCORBA.PriorityMapping.Linear;
with RTCORBA.PriorityModelPolicy;
with RTCORBA.ThreadpoolPolicy;

with RTPortableServer.POA;

with PolyORB.RTCORBA_P.Setup;

with Echo.Impl;

with PolyORB.Utils.Report;

--  Begin of PolyORB's setup

with PolyORB.ORB.Thread_Pool;
pragma Warnings (Off, PolyORB.ORB.Thread_Pool);
pragma Elaborate_All (PolyORB.ORB.Thread_Pool);

with PolyORB.ORB_Controller.Half_Sync_Half_Async;
pragma Warnings (Off, PolyORB.ORB_Controller.Half_Sync_Half_Async);
pragma Elaborate_All (PolyORB.ORB_Controller.Half_Sync_Half_Async);

with PolyORB.Request_Scheduler.Servant_Lane;
pragma Warnings (Off, PolyORB.Request_Scheduler.Servant_Lane);
pragma Elaborate_All (PolyORB.Request_Scheduler.Servant_Lane);

with PolyORB.Setup.Tasking.Full_Tasking;
pragma Warnings (Off, PolyORB.Setup.Tasking.Full_Tasking);
pragma Elaborate_All (PolyORB.Setup.Tasking.Full_Tasking);

with PolyORB.Parameters.File;
pragma Warnings (Off, PolyORB.Parameters.File);
pragma Elaborate_All (PolyORB.Parameters.File);

with PolyORB.Setup.OA.Basic_RT_POA;
pragma Warnings (Off, PolyORB.Setup.OA.Basic_RT_POA);
pragma Elaborate_All (PolyORB.Setup.OA.Basic_RT_POA);

with PolyORB.Setup.IIOP;
pragma Elaborate_All (PolyORB.Setup.IIOP);
pragma Warnings (Off, PolyORB.Setup.IIOP);

with PolyORB.Setup.Access_Points.IIOP;
pragma Elaborate_All (PolyORB.Setup.Access_Points.IIOP);
pragma Warnings (Off, PolyORB.Setup.Access_Points.IIOP);

with PolyORB.GIOP_P.Tagged_Components.Policies.Priority_Model_Policy;
pragma Elaborate_All
  (PolyORB.GIOP_P.Tagged_Components.Policies.Priority_Model_Policy);
pragma Warnings
  (Off, PolyORB.GIOP_P.Tagged_Components.Policies.Priority_Model_Policy);

--  End of PolyORB's setup

procedure Server is

   use Ada.Text_IO;

   use CORBA.ORB;
   use CORBA.Policy.IDL_Sequence_Policy;

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

      RT_ORB : RTCORBA.RTORB.Ref;

      Root_POA : PortableServer.POA.Ref;

      --  Variables for Child_POA #1

      Obj_Server_1 : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;

      Priority_Model_Policy_Ref_1 : RTCORBA.PriorityModelPolicy.Ref;

      Thread_Pool_Id_1 : RTCORBA.ThreadpoolId;

      Lanes : RTCORBA.ThreadpoolLanes;

      Thread_Pool_Policy_Ref_1 : RTCORBA.ThreadpoolPolicy.Ref;
      Policies_1 : CORBA.Policy.PolicyList;
      Child_POA_Server_1 : RTPortableServer.POA.Ref;

      Ref_Server_1 : CORBA.Object.Ref;

      Default_Priority_1 : constant RTCORBA.Priority := 10000;
      Default_Priority_2 : constant RTCORBA.Priority := 20000;

   begin

      --  Retrieve RT ORB

      RT_ORB := RTCORBA.RTORB.To_Ref
        (Resolve_Initial_References
         (To_CORBA_String ("RTORB")));

      Output ("Retrieved reference on RT ORB", True);

      --  Retrieve Root POA

      Root_POA := PortableServer.POA.To_Ref
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
         Default_Priority_1);

      Output ("CLIENT_PROPAGATED policy declared", True);

      --  Create Lane

      Append (Lanes,
               RTCORBA.ThreadpoolLane'(Lane_Priority => Default_Priority_1,
                                       Static_Threads => 2,
                                       Dynamic_Threads => 0));

      Append (Lanes,
               RTCORBA.ThreadpoolLane'(Lane_Priority => Default_Priority_2,
                                       Static_Threads => 2,
                                       Dynamic_Threads => 0));

      Output ("Lanes created", True);

      --  Construct Thread Pool policy from previous threadpool

      Thread_Pool_Id_1 := Create_Threadpool_With_Lanes
        (RT_ORB,
         1,
         Lanes,
         False,
         False,
         1,
         0);

      Thread_Pool_Policy_Ref_1 := RTCORBA.RTORB.Create_Threadpool_Policy
           (RT_ORB, Thread_Pool_Id_1);
      Output ("Create Threadpool policy", True);

      --  Create Child POA with CLIENT_PROPAGATED priority model policy

      Append (Policies_1,
              CORBA.Policy.Ref (Priority_Model_Policy_Ref_1));

      Append (Policies_1,
              CORBA.Policy.Ref (Thread_Pool_Policy_Ref_1));

      Child_POA_Server_1 := RTPortableServer.POA.To_Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          CORBA.To_CORBA_String ("Child_POA_Server_1"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies_1));

      Output ("Create Child POA with these policies", True);

      --  Set up new object and attach it to Child_POA

      Ref_Server_1 := PortableServer.POA.Servant_To_Reference
        (PortableServer.POA.Ref (Child_POA_Server_1),
         PortableServer.Servant (Obj_Server_1));

      Output ("Implicit activation of an object with these policies", True);

      --  Output object IOR

      Put_Line ("IOR of object #1, attached to thread pool with 2 lanes "
                & "with priorities"
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
