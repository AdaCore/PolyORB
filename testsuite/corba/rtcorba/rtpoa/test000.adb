------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
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

with PolyORB.ORB.Thread_Pool;
pragma Warnings (Off, PolyORB.ORB.Thread_Pool);

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

   declare
      use CORBA.Policy.IDL_SEQUENCE_Policy;

      RT_ORB : RTCORBA.RTORB.Local_Ref;

      Root_POA : PortableServer.POA.Local_Ref;

      procedure Test_SERVER_DECLARED_1;
      procedure Test_SERVER_DECLARED_2;
      procedure Test_CLIENT_PROPAGATED_1;
      procedure Test_CLIENT_PROPAGATED_2;

      ----------------------------
      -- Test_SERVER_DECLARED_1 --
      ----------------------------

      procedure Test_SERVER_DECLARED_1 is
         Obj_Server : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;
         Priority_Model_Policy_Ref_Server :
           RTCORBA.PriorityModelPolicy.Local_Ref;
         Child_POA_Server : RTPortableServer.POA.Local_Ref;
         Policies_Server : CORBA.Policy.PolicyList;
         Ref_Server : CORBA.Object.Ref;

      begin
         New_Test ("SERVER_DECLARED POA #1");

         --  Set up SERVER_DECLARED priority model policy

         Priority_Model_Policy_Ref_Server
           := Create_Priority_Model_Policy
           (RT_ORB,
            SERVER_DECLARED,
            10000);

         Output ("SERVER_DECLARED policy declared", True);

         --  Create Child POA with SERVER_DECLARED priority model policy

         Append (Policies_Server,
                 CORBA.Policy.Ref (Priority_Model_Policy_Ref_Server));

         Child_POA_Server := RTPortableServer.POA.Helper.To_Local_Ref
           (PortableServer.POA.Create_POA
            (Root_POA,
             CORBA.To_CORBA_String ("Child_POA_Server"),
             PortableServer.POA.Get_The_POAManager (Root_POA),
             Policies_Server));

         --  Set up new object and attach it to Child_POA

         Ref_Server := PortableServer.POA.Servant_To_Reference
           (PortableServer.POA.Local_Ref (Child_POA_Server),
            PortableServer.Servant (Obj_Server));

         Output ("Implicit activation of an object with SERVER_DECLARED "
                 & "policy", True);

         --  Output object IOR

         New_Line;
         Put_Line
           ("'"
            & CORBA.To_Standard_String
            (CORBA.Object.Object_To_String (Ref_Server))
            & "'");
         New_Line;

         --  Create reference with priority

         begin
            declare
               Obj_Ref2 : constant CORBA.Object.Ref :=
                 RTPortableServer.POA.Create_Reference_With_Priority
                 (Child_POA_Server,
                  CORBA.To_CORBA_String (Echo.Repository_Id),
                  10000);
               pragma Unreferenced (Obj_Ref2);
            begin
               Output ("Create_Reference_With_Priority raised an exception",
                       False);
            end;
         exception
            when PortableServer.POA.WrongPolicy =>
               Output ("Create_Reference_With_Priority raised an exception",
                       True);
         end;

         --  Create reference with id and priority

         begin
            declare
               Oid : constant PortableServer.ObjectId
                 := PortableServer.String_To_ObjectId ("dead");

               Obj_Ref2 : constant CORBA.Object.Ref :=
                 RTPortableServer.POA.Create_Reference_With_Id_And_Priority
                 (Child_POA_Server,
                  Oid,
                  CORBA.To_CORBA_String (Echo.Repository_Id),
                  10000);
               pragma Unreferenced (Obj_Ref2);
            begin
               Output
                 ("Create_Reference_With_Id_And_Priority raised an exception",
                  False);
            end;
         exception
            when PortableServer.POA.WrongPolicy =>
               Output
                 ("Create_Reference_With_Id_And_Priority raised an exception",
                  True);
         end;

         --  Activate servant with priority

         begin
            declare
               Obj2 : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;

               Oid : constant PortableServer.ObjectId
                 := RTPortableServer.POA.Activate_Object_With_Priority
                 (Child_POA_Server,
                  PortableServer.Servant (Obj2),
                  10000);
               pragma Unreferenced (Oid);
            begin
               Output ("Activate_Object_With_Priority raised no exception",
                       False);
            end;
         exception
            when PortableServer.POA.WrongPolicy =>
            Output ("Activate_Object_With_Priority raised an exception", True);
         end;

         --  Activate servant with id and priority

         begin
            declare
               Obj2 : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;

               Oid : constant PortableServer.ObjectId
                 := PortableServer.String_To_ObjectId ("dead");

            begin
               RTPortableServer.POA.Activate_Object_With_Id_And_Priority
                 (Child_POA_Server,
                  Oid,
                  PortableServer.Servant (Obj2),
                  10000);

               Output
                 ("Activate_Object_With_Id_And_Priority raised no exception",
                  False);
            end;
         exception
            when PortableServer.POA.WrongPolicy =>
               Output
                 ("Activate_Object_With_Id_And_Priority raised no exception",
                  True);
         end;

         Destroy (PortableServer.POA.Local_Ref (Child_POA_Server),
                  False, False);
         Output ("All tests done", True);
      end Test_SERVER_DECLARED_1;

      ----------------------------
      -- Test_SERVER_DECLARED_2 --
      ----------------------------

      procedure Test_SERVER_DECLARED_2 is
         Obj_Server : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;
         Priority_Model_Policy_Ref_Server :
           RTCORBA.PriorityModelPolicy.Local_Ref;
         Child_POA_Server : RTPortableServer.POA.Local_Ref;
         Policies_Server : CORBA.Policy.PolicyList;
         Ref_Server : CORBA.Object.Ref;
         Thread_Pool_Id : RTCORBA.ThreadpoolId;
         Thread_Pool_Policy_Ref : RTCORBA.ThreadpoolPolicy.Local_Ref;

         No_Implicit_Activation_Policy : constant CORBA.Policy.Ref
           := CORBA.Policy.Ref
           (Create_Implicit_Activation_Policy (NO_IMPLICIT_ACTIVATION));

      begin
         New_Test ("SERVER_DECLARED POA #2");

         --  Set up SERVER_DECLARED priority model policy

         Priority_Model_Policy_Ref_Server
           := Create_Priority_Model_Policy
           (RT_ORB,
            SERVER_DECLARED,
            12000);

         Output ("SERVER_DECLARED policy declared", True);

         --  Create Threadpool

         Thread_Pool_Id := RTCORBA.RTORB.Create_Threadpool
           (RT_ORB,
            Stacksize               => 262_144,
            Static_Threads          => 2,
            Dynamic_Threads         => 0,
            Default_Priority        => 12000,
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
         --  and NO_IMPLICIT_ACTIVATION activation policy

         Append (Policies_Server,
                 CORBA.Policy.Ref (Priority_Model_Policy_Ref_Server));

         Append (Policies_Server, CORBA.Policy.Ref (Thread_Pool_Policy_Ref));

         Append (Policies_Server, No_Implicit_Activation_Policy);

         Output ("NO_IMPLICIT_ACTIVATION policy declared", True);

         Child_POA_Server := RTPortableServer.POA.Helper.To_Local_Ref
           (PortableServer.POA.Create_POA
            (Root_POA,
             CORBA.To_CORBA_String ("Child_POA_Server"),
             PortableServer.POA.Get_The_POAManager (Root_POA),
             Policies_Server));

         Output ("Created child POA with SERVER_DECLARED policy", True);

         --  Set up new object and attach it to Child_POA

         begin
            Ref_Server := PortableServer.POA.Servant_To_Reference
              (PortableServer.POA.Local_Ref (Child_POA_Server),
               PortableServer.Servant (Obj_Server));

            Output ("Created object with SERVER_DECLARED policy "
                    & "raised no exception", False);
         exception
            when PortableServer.POA.ServantNotActive =>
               Output ("Created object with SERVER_DECLARED policy "
                       & "raised PortableServer.POA.ServantNotActive", True);
         end;

         --  Create reference with priority

         begin
            declare
               Obj_Ref2 : constant CORBA.Object.Ref :=
                 RTPortableServer.POA.Create_Reference_With_Priority
                 (Child_POA_Server,
                  CORBA.To_CORBA_String (Echo.Repository_Id),
                  7);
               pragma Unreferenced (Obj_Ref2);
            begin
               Output ("Create_Reference_With_Priority raised an exception",
                       False);
            end;
         exception
            when CORBA.Bad_Param =>
               Output ("Create_Reference_With_Priority raised an exception",
                       True);
         end;

         --  Create reference

         begin
            declare
               Obj_Ref2 : constant CORBA.Object.Ref :=
                 RTPortableServer.POA.Create_Reference_With_Priority
                 (Child_POA_Server,
                  CORBA.To_CORBA_String (Echo.Repository_Id),
                  12000);

            begin
               Output ("Create_Reference_With_Priority raised no exception",
                       True);

               --  Output object IOR

               New_Line;
               Put_Line
                 ("'"
                  & CORBA.To_Standard_String
                  (CORBA.Object.Object_To_String (Obj_Ref2))
                  & "'");
               New_Line;

            end;
         exception
            when others =>
               Output ("Create_Reference_With_Priority raised no exception",
                       False);
               raise;
         end;

         --  Activate servant with priority

         begin
            declare
               Obj2 : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;

               Oid : constant PortableServer.ObjectId
                 := RTPortableServer.POA.Activate_Object_With_Priority
                 (Child_POA_Server,
                  PortableServer.Servant (Obj2),
                  12000);

            begin
               Output ("Activate_Object_With_Priority did not raise exception",
                       True);

               --  Call Servant_To_Reference

               Ref_Server := PortableServer.POA.Id_To_Reference
                 (PortableServer.POA.Local_Ref (Child_POA_Server), Oid);

               --  Output object IOR

               New_Line;
               Put_Line
                 ("'"
                  & CORBA.To_Standard_String
                  (CORBA.Object.Object_To_String (Ref_Server))
                  & "'");
               New_Line;

            end;
         exception
            when PortableServer.POA.WrongPolicy =>
               Output ("Activate_Object_With_Priority raise exception", False);
         end;

         Destroy (PortableServer.POA.Local_Ref (Child_POA_Server),
                  False, False);
         Output ("All tests done", True);
      end Test_SERVER_DECLARED_2;

      ------------------------------
      -- Test_CLIENT_PROPAGATED_1 --
      ------------------------------

      procedure Test_CLIENT_PROPAGATED_1 is
         Obj_Client : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;
         Priority_Model_Policy_Ref_Client :
           RTCORBA.PriorityModelPolicy.Local_Ref;
         Child_POA_Client : RTPortableServer.POA.Local_Ref;
         Policies_Client : CORBA.Policy.PolicyList;
         Ref_Client : CORBA.Object.Ref;

      begin
         New_Test ("CLIENT_PROPAGATED POA #1");

         --  Set up CLIENT_PROPAGATED priority model policy

         Priority_Model_Policy_Ref_Client
           := Create_Priority_Model_Policy
           (RT_ORB,
            CLIENT_PROPAGATED,
            14000);

         Output ("CLIENT_PROPAGATED policy declared", True);

         --  Create Child POA with CLIENT_PROPAGATED priority model policy

         Append (Policies_Client,
                 CORBA.Policy.Ref (Priority_Model_Policy_Ref_Client));

         Child_POA_Client := RTPortableServer.POA.Helper.To_Local_Ref
           (PortableServer.POA.Create_POA
            (Root_POA,
             CORBA.To_CORBA_String ("Child_POA_Client"),
             PortableServer.POA.Get_The_POAManager (Root_POA),
             Policies_Client));

         Output ("Created child POA with CLIENT_PROPAGATED policy", True);

         --  Set up new object and attach it to Child_POA

         Ref_Client := PortableServer.POA.Servant_To_Reference
           (PortableServer.POA.Local_Ref (Child_POA_Client),
            PortableServer.Servant (Obj_Client));
         Output ("Implicit activation of an object with CLIENT_PROPAGATED "
                 & "policy", True);

         --  Output object IOR

         New_Line;
         Put_Line
           ("'"
            & CORBA.To_Standard_String
            (CORBA.Object.Object_To_String (Ref_Client))
            & "'");
         New_Line;

         --  Create reference

         begin
            declare
               Obj_Ref2 : constant CORBA.Object.Ref :=
                 RTPortableServer.POA.Create_Reference_With_Priority
                 (Child_POA_Client,
                  CORBA.To_CORBA_String (Echo.Repository_Id),
                  7);
               pragma Unreferenced (Obj_Ref2);
            begin
               Output ("Create_Reference_With_Priority raised an exception",
                       False);
            end;
         exception
            when PortableServer.POA.WrongPolicy =>
               Output ("Create_Reference_With_Priority raised an exception",
                       True);
         end;

         --  Activate servant with priority

         begin
            declare
               Obj2 : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;

               Oid : constant PortableServer.ObjectId
                 := RTPortableServer.POA.Activate_Object_With_Priority
                 (Child_POA_Client,
                  PortableServer.Servant (Obj2),
                  7);
               pragma Unreferenced (Oid);
            begin
               Output ("Activate_Object_With_Priority raised no exception",
                       False);

            end;

         exception
            when PortableServer.POA.WrongPolicy =>
               Output ("Activate_Object_With_Priority raised an exception",
                       True);
         end;

         --  Activate servant with id & priority

         begin
            declare
               Obj2 : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;

               Oid : constant PortableServer.ObjectId
                 := PortableServer.String_To_ObjectId ("dead");

            begin
               RTPortableServer.POA.Activate_Object_With_Id_And_Priority
                 (Child_POA_Client,
                  Oid,
                  PortableServer.Servant (Obj2),
                  7);

               Output
                 ("Activate_Object_With_id_and_Priority raised no exception",
                  False);
            end;
         exception
            when PortableServer.POA.WrongPolicy =>
               Output
                 ("Activate_Object_With_id_and_Priority raised no exception",
                  True);
         end;

         Destroy (PortableServer.POA.Local_Ref (Child_POA_Client),
                  False, False);
         Output ("All tests done", True);
      end Test_CLIENT_PROPAGATED_1;

      ------------------------------
      -- Test_CLIENT_PROPAGATED_2 --
      ------------------------------

      procedure Test_CLIENT_PROPAGATED_2 is
         Obj_Client : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;
         Priority_Model_Policy_Ref_Client :
           RTCORBA.PriorityModelPolicy.Local_Ref;
         Child_POA_Client : RTPortableServer.POA.Local_Ref;
         Policies_Client : CORBA.Policy.PolicyList;
         Ref_Client : CORBA.Object.Ref;
         --  pragma Unreferenced (Ref_Client);
         pragma Warnings (Off, Ref_Client); --  WAG:5.02 DB08-008
         --  Assigned but never read

         No_Implicit_Activation_Policy : constant CORBA.Policy.Ref
           := CORBA.Policy.Ref
           (Create_Implicit_Activation_Policy (NO_IMPLICIT_ACTIVATION));

      begin
         New_Test ("CLIENT_PROPAGATED POA #2");

         --  Set up CLIENT_PROPAGATED priority model policy

         Priority_Model_Policy_Ref_Client
           := Create_Priority_Model_Policy
           (RT_ORB,
            CLIENT_PROPAGATED,
            8);

         Output ("CLIENT_PROPAGATED policy declared", True);

         --  Create Child POA with CLIENT_PROPAGATED priority model policy
         --  and NO_IMPLICIT_ACTIVATION activation policy

         Append (Policies_Client,
                 CORBA.Policy.Ref (Priority_Model_Policy_Ref_Client));

         Append (Policies_Client, No_Implicit_Activation_Policy);

         Output ("NO_IMPLICIT_ACTIVATION policy declared", True);

         Child_POA_Client := RTPortableServer.POA.Helper.To_Local_Ref
           (PortableServer.POA.Create_POA
            (Root_POA,
             CORBA.To_CORBA_String ("Child_POA_Client"),
             PortableServer.POA.Get_The_POAManager (Root_POA),
             Policies_Client));

         Output ("Created child POA with CLIENT_PROPAGATED policy", True);

         --  Set up new object and attach it to Child_POA

         begin
            Ref_Client := PortableServer.POA.Servant_To_Reference
              (PortableServer.POA.Local_Ref (Child_POA_Client),
               PortableServer.Servant (Obj_Client));
            Output ("Creating object with CLIENT_PROPAGATED policy "
                    & "raised no exception", False);
         exception
            when PortableServer.POA.ServantNotActive =>
               Output ("Created object with CLIENT_PROPAGATED policy "
                       & "raised PortableServer.POA.ServantNotActive", True);
         end;

         --  Activate servant with priority

         begin
            declare
               Oid : constant PortableServer.ObjectId
                 := RTPortableServer.POA.Activate_Object_With_Priority
                 (Child_POA_Client, PortableServer.Servant (Obj_Client), 8);
               pragma Unreferenced (Oid);

            begin
               Output ("Activate_Object_With_Priority did not raise exception",
                       False);
            end;
         exception
            when PortableServer.POA.WrongPolicy =>
               Output ("Activate_Object_With_Priority raise exception", True);
         end;

         Destroy (PortableServer.POA.Local_Ref (Child_POA_Client),
                  False, False);
         Output ("All tests done", True);
      end Test_CLIENT_PROPAGATED_2;

   begin
      New_Test ("RTPOA");

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

      Test_SERVER_DECLARED_1;
      Test_SERVER_DECLARED_2;

      Test_CLIENT_PROPAGATED_1;
      Test_CLIENT_PROPAGATED_2;

      End_Report;
   end;
exception
   when E : others =>
      New_Line;
      Put_Line ("Got exception: "
                & Ada.Exceptions.Exception_Information (E));

      Output ("Test failed", False);
      End_Report;
end Test000;
