------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
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
with RTPortableServer.POA;

with PolyORB.RTCORBA_P.Setup;

with Echo.Impl;

with PolyORB.Utils.Report;

--  Begin of PolyORB's setup

with PolyORB.ORB.No_Tasking;
pragma Warnings (Off, PolyORB.ORB.No_Tasking);
pragma Elaborate_All (PolyORB.ORB.No_Tasking);

with PolyORB.ORB_Controller.Basic;
pragma Warnings (Off, PolyORB.ORB_Controller.Basic);
pragma Elaborate_All (PolyORB.ORB_Controller.Basic);

with PolyORB.Request_Scheduler.Servant_Lane;
pragma Warnings (Off, PolyORB.Request_Scheduler.Servant_Lane);
pragma Elaborate_All (PolyORB.Request_Scheduler.Servant_Lane);

with PolyORB.Setup.Tasking.No_Tasking;
pragma Warnings (Off, PolyORB.Setup.Tasking.No_Tasking);
pragma Elaborate_All (PolyORB.Setup.Tasking.No_Tasking);

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
      use CORBA.Policy.IDL_Sequence_Policy;

      RT_ORB : RTCORBA.RTORB.Ref;

      Root_POA : PortableServer.POA.Ref;

   begin
      New_Test ("RTPOA");

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

      declare
         Obj_Server : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;
         Priority_Model_Policy_Ref_Server : RTCORBA.PriorityModelPolicy.Ref;
         Child_POA_Server : RTPortableServer.POA.Ref;
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

         Child_POA_Server := RTPortableServer.POA.To_Ref
           (PortableServer.POA.Create_POA
            (Root_POA,
             CORBA.To_CORBA_String ("Child_POA_Server"),
             PortableServer.POA.Get_The_POAManager (Root_POA),
             Policies_Server));

         --  Set up new object and attach it to Child_POA

         Ref_Server := PortableServer.POA.Servant_To_Reference
           (PortableServer.POA.Ref (Child_POA_Server),
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

         --  Activate servant with priority

         begin
            declare
               Obj2 : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;

               Oid : constant PortableServer.ObjectId
                 := RTPortableServer.POA.Activate_Object_With_Priority
                 (Child_POA_Server,
                  PortableServer.Servant (Obj2),
                  11000);

            begin
               Output ("Activate_Object_With_Priority did not raise exception",
                       True);

               --  Call Servant_To_Reference

               Ref_Server := PortableServer.POA.Id_To_Reference
                 (PortableServer.POA.Ref (Child_POA_Server), Oid);

               --  Output object IOR

               New_Line;
               Put_Line
                 ("'"
                  & CORBA.To_Standard_String
                  (CORBA.Object.Object_To_String (Ref_Server))
                  & "'");
               New_Line;

            end;

            Output ("Activate_Object_With_Priority raised no exception",
                    False);
         exception
            when PortableServer.POA.WrongPolicy =>
            Output ("Activate_Object_With_Priority raised an exception", True);
         end;

         Destroy (PortableServer.POA.Ref (Child_POA_Server), False, False);
      end;

      declare
         Obj_Server : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;
         Priority_Model_Policy_Ref_Server : RTCORBA.PriorityModelPolicy.Ref;
         Child_POA_Server : RTPortableServer.POA.Ref;
         Policies_Server : CORBA.Policy.PolicyList;
         Ref_Server : CORBA.Object.Ref;


         Implicit_Activation_Policy : CORBA.Policy.Ref
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

         --  Create Child POA with SERVER_DECLARED priority model policy
         --  and NO_IMPLICIT_ACTIVATION activation policy

         Append (Policies_Server,
                 CORBA.Policy.Ref (Priority_Model_Policy_Ref_Server));

         Append (Policies_Server, Implicit_Activation_Policy);

         Child_POA_Server := RTPortableServer.POA.To_Ref
           (PortableServer.POA.Create_POA
            (Root_POA,
             CORBA.To_CORBA_String ("Child_POA_Server"),
             PortableServer.POA.Get_The_POAManager (Root_POA),
             Policies_Server));

         Output ("Created child POA with SERVER_DECLARED policy", True);

         --  Set up new object and attach it to Child_POA

         begin
            Ref_Server := PortableServer.POA.Servant_To_Reference
              (PortableServer.POA.Ref (Child_POA_Server),
               PortableServer.Servant (Obj_Server));

            Output ("Created object with SERVER_DECLARED policy "
                    & "raised no exception", False);
         exception
            when PortableServer.POA.ServantNotActive =>
               Output ("Created object with SERVER_DECLARED policy "
                       & "raised PortableServer.POA.ServantNotActive", True);
         end;

         --  Activate servant with priority

         begin
            declare
               Obj2 : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;

               Oid : constant PortableServer.ObjectId
                 := RTPortableServer.POA.Activate_Object_With_Priority
                 (Child_POA_Server,
                  PortableServer.Servant (Obj2),
                  13000);

            begin
               Output ("Activate_Object_With_Priority did not raise exception",
                       True);

               --  Call Servant_To_Reference

               Ref_Server := PortableServer.POA.Id_To_Reference
                 (PortableServer.POA.Ref (Child_POA_Server), Oid);

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
               raise;
         end;

         Destroy (PortableServer.POA.Ref (Child_POA_Server), False, False);
      end;

      declare
         Obj_Client : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;
         Priority_Model_Policy_Ref_Client : RTCORBA.PriorityModelPolicy.Ref;
         Child_POA_Client : RTPortableServer.POA.Ref;
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

         Child_POA_Client := RTPortableServer.POA.To_Ref
           (PortableServer.POA.Create_POA
            (Root_POA,
             CORBA.To_CORBA_String ("Child_POA_Client"),
             PortableServer.POA.Get_The_POAManager (Root_POA),
             Policies_Client));

         Output ("Created child POA with CLIENT_PROPAGATED policy", True);

         --  Set up new object and attach it to Child_POA

         Ref_Client := PortableServer.POA.Servant_To_Reference
           (PortableServer.POA.Ref (Child_POA_Client),
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

         --  Activate servant with priority

         begin
            declare
               Obj2 : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;

               Oid : constant PortableServer.ObjectId
                 := RTPortableServer.POA.Activate_Object_With_Priority
                 (Child_POA_Client,
                  PortableServer.Servant (Obj2),
                  7);

            begin
               Output ("Activate_Object_With_Priority did not raise exception",
                       True);

               --  Call Servant_To_Reference

               Ref_Client := PortableServer.POA.Id_To_Reference
                 (PortableServer.POA.Ref (Child_POA_Client), Oid);

               --  Output object IOR

               New_Line;
               Put_Line
                 ("'"
                  & CORBA.To_Standard_String
                  (CORBA.Object.Object_To_String (Ref_Client))
                  & "'");
               New_Line;

            end;

            Output ("Activate_Object_With_Priority raised no exception",
                    False);
         exception
            when PortableServer.POA.WrongPolicy =>
            Output ("Activate_Object_With_Priority raised an exception", True);
         end;

         Destroy (PortableServer.POA.Ref (Child_POA_Client), False, False);
      end;

      declare
         Obj_Client : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;
         Priority_Model_Policy_Ref_Client : RTCORBA.PriorityModelPolicy.Ref;
         Child_POA_Client : RTPortableServer.POA.Ref;
         Policies_Client : CORBA.Policy.PolicyList;
         Ref_Client : CORBA.Object.Ref;

         Implicit_Activation_Policy : CORBA.Policy.Ref
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

         Append (Policies_Client, Implicit_Activation_Policy);

         Child_POA_Client := RTPortableServer.POA.To_Ref
           (PortableServer.POA.Create_POA
            (Root_POA,
             CORBA.To_CORBA_String ("Child_POA_Client"),
             PortableServer.POA.Get_The_POAManager (Root_POA),
             Policies_Client));

         Output ("Created child POA with CLIENT_PROPAGATED policy", True);

         --  Set up new object and attach it to Child_POA

         begin
            Ref_Client := PortableServer.POA.Servant_To_Reference
              (PortableServer.POA.Ref (Child_POA_Client),
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

         Destroy (PortableServer.POA.Ref (Child_POA_Client), False, False);
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
