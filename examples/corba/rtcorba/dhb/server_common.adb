------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        S E R V E R _ C O M M O N                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2006-2007, Free Software Foundation, Inc.        --
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
with PolyORB.Utils.Report;

with DHB.Worker;
with DHB.Worker_Factory.Helper;
with DHB.Worker_Factory.Impl;

with DHB.Background_Worker_Factory.Helper;
with DHB.Background_Worker_Factory.Impl;

with DHB.Background_Worker.Impl;
pragma Warnings (Off, DHB.Background_Worker.Impl);

with Utils;
with Whetstone;

package body Server_Common is

   ----------------
   -- Run_Server --
   ----------------

   procedure Run_Server is
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

      Output ("Node KWIPS =" & Positive'Image (Whetstone.Compute_KWIPS), True);

      declare
         use RTCORBA.IDL_SEQUENCE_RTCORBA_ThreadpoolLane;

         RT_ORB : RTCORBA.RTORB.Local_Ref;

         Root_POA : PortableServer.POA.Local_Ref;

         --  Variables for Child_POA

         Priority_Model_Policy_Ref : RTCORBA.PriorityModelPolicy.Local_Ref;

         Thread_Pool_Id : RTCORBA.ThreadpoolId;

         Lanes : RTCORBA.ThreadpoolLanes;

         Thread_Pool_Policy_Ref : RTCORBA.ThreadpoolPolicy.Local_Ref;
         Policies : CORBA.Policy.PolicyList;
         Child_POA : RTPortableServer.POA.Local_Ref;

         Base_Priority      : constant RTCORBA.Priority := 1_000;
         Default_Priority   : constant RTCORBA.Priority := 10_000;
         Default_Priority_2 : constant RTCORBA.Priority := 20_000;

         Worker_Factory_Object : constant CORBA.Impl.Object_Ptr
           := new DHB.Worker_Factory.Impl.Object;
         Worker_Factory_Ref : DHB.Worker_Factory.Ref;
         Worker_Ref : DHB.Worker.Ref;

         Background_Worker_Factory_Object : constant CORBA.Impl.Object_Ptr
           := new DHB.Background_Worker_Factory.Impl.Object;
         Background_Worker_Factory_Ref : DHB.Background_Worker_Factory.Ref;
         Background_Worker_Ref : DHB.Background_Worker.Ref;
      begin
         --  Retrieve RT ORB

         RT_ORB := RTCORBA.RTORB.Helper.To_Local_Ref
           (Resolve_Initial_References
            (To_CORBA_String ("RTORB")));

         Output ("Retrieved reference on RTORB", True);

         --  Retrieve Root POA

         Root_POA := PortableServer.POA.Helper.To_Local_Ref
           (CORBA.ORB.Resolve_Initial_References
            (CORBA.ORB.To_CORBA_String ("RootPOA")));

         PortableServer.POAManager.Activate
           (PortableServer.POA.Get_The_POAManager (Root_POA));

         Output ("Retrieved and activated Root POA", True);

         New_Test ("Setting up Child_POA #1");

         --  Create CLIENT_PROPAGATED PriorityModel policy

         Priority_Model_Policy_Ref
           := Create_Priority_Model_Policy
           (RT_ORB,
            CLIENT_PROPAGATED,
            Base_Priority);

         Output ("CLIENT_PROPAGATED policy declared", True);

         --  Create Lanes

         Append (Lanes,
                 RTCORBA.ThreadpoolLane'(Lane_Priority => Base_Priority,
                                         Static_Threads => 2,
                                         Dynamic_Threads => 0));

         Append (Lanes,
                 RTCORBA.ThreadpoolLane'(Lane_Priority => Default_Priority,
                                         Static_Threads => 2,
                                         Dynamic_Threads => 0));

         Append (Lanes,
                 RTCORBA.ThreadpoolLane'(Lane_Priority => Default_Priority_2,
                                         Static_Threads => 2,
                                         Dynamic_Threads => 0));

         Output ("Lanes created", True);

         --  Construct Thread Pool policy from previous threadpool

         Thread_Pool_Id := Create_Threadpool_With_Lanes
           (RT_ORB,
            Stacksize               => 262_144,
            Lanes                   => Lanes,
            Allow_Borrowing         => False,
            Allow_Request_Buffering => False,
            Max_Buffered_Requests   => 1,
            Max_Request_Buffer_Size => 0);

         Thread_Pool_Policy_Ref := RTCORBA.RTORB.Create_Threadpool_Policy
           (RT_ORB, Thread_Pool_Id);
         Output ("Create Threadpool policy", True);

         --  Create Child POA with CLIENT_PROPAGATED priority model policy

         Append (Policies,
                 CORBA.Policy.Ref (Priority_Model_Policy_Ref));

         Append (Policies,
                 CORBA.Policy.Ref (Thread_Pool_Policy_Ref));

         Child_POA := RTPortableServer.POA.Helper.To_Local_Ref
           (PortableServer.POA.Create_POA
            (Root_POA,
             CORBA.To_CORBA_String ("Child_POA"),
             PortableServer.POA.Get_The_POAManager (Root_POA),
             Policies));

         Output ("Create Child POA with these policies", True);

         --  Set up Worker_Factory object and attach it to Child_POA

         DHB.Worker_Factory.Impl.Initialize
           (DHB.Worker_Factory.Impl.Object (Worker_Factory_Object.all)'Access,
            Child_POA);

         Worker_Factory_Ref := DHB.Worker_Factory.Helper.To_Ref
           (PortableServer.POA.Servant_To_Reference
            (PortableServer.POA.Local_Ref (Child_POA),
             PortableServer.Servant (Worker_Factory_Object)));

         Output ("Set up Worker_Factory", True);

         --  Output information on lanes

         Put_Line
           ("IOR of Worker factory, attached to thread pool with 3 lanes "
            & "with priorities:");
         Put_Line ("-" & RTCORBA.Priority'Image (Base_Priority));
         Put_Line ("-" & RTCORBA.Priority'Image (Default_Priority));
         Put_Line ("-" & RTCORBA.Priority'Image (Default_Priority_2));

         --  Output object IOR

         New_Line;
         Put_Line ("Worker_Factory IOR:");
         Put_Line
           ("'"
            & CORBA.To_Standard_String
            (CORBA.Object.Object_To_String (Worker_Factory_Ref))
            & "'");
         New_Line;

         --  Create a Worker

         Worker_Ref := DHB.Worker_Factory.Create
           (DHB.Worker_Factory.Helper.To_Ref (Worker_Factory_Ref));
         Output ("Set up Worker", True);

         New_Line;
         Put_Line ("Worker IOR:");
         Put_Line
           ("'"
            & CORBA.To_Standard_String
            (CORBA.Object.Object_To_String (Worker_Ref))
            & "'");
         New_Line;

         Utils.Put_Ref ("worker.ior", Worker_Ref);
         Put_Line ("Ready.");

         --  Set up Background_Worker_Factory object and attach it to Child_POA

         DHB.Background_Worker_Factory.Impl.Initialize
           (DHB.Background_Worker_Factory.Impl.Object
            (Background_Worker_Factory_Object.all)'Access,
            Child_POA);

         Background_Worker_Factory_Ref
           := DHB.Background_Worker_Factory.Helper.To_Ref
           (PortableServer.POA.Servant_To_Reference
            (PortableServer.POA.Local_Ref (Child_POA),
             PortableServer.Servant (Background_Worker_Factory_Object)));

         Output ("Set up Background_Worker_Factory", True);

         --  Output information on lanes

         Put_Line
           ("IOR of Worker factory, attached to thread pool with 3 lanes "
            & "with priorities:");
         Put_Line ("-" & RTCORBA.Priority'Image (Base_Priority));
         Put_Line ("-" & RTCORBA.Priority'Image (Default_Priority));
         Put_Line ("-" & RTCORBA.Priority'Image (Default_Priority_2));

         --  Output object IOR

         New_Line;
         Put_Line ("Background_Worker_Factory IOR:");
         Put_Line
           ("'"
            & CORBA.To_Standard_String
            (CORBA.Object.Object_To_String (Background_Worker_Factory_Ref))
            & "'");
         New_Line;

         --  Create a Background_Worker

         Background_Worker_Ref := DHB.Background_Worker_Factory.Create
           (DHB.Background_Worker_Factory.Helper.To_Ref
            (Background_Worker_Factory_Ref));
         Output ("Set up Background_Worker", True);

         New_Line;
         Put_Line ("Background_Worker IOR:");
         Put_Line
           ("'"
            & CORBA.To_Standard_String
            (CORBA.Object.Object_To_String (Background_Worker_Ref))
            & "'");
         New_Line;

         Utils.Put_Ref ("background_worker.ior", Background_Worker_Ref);

         --  Launch the server

         CORBA.ORB.Run;
      end;
   end Run_Server;

end Server_Common;
