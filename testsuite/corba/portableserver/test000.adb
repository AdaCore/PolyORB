------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2003 Free Software Fundation              --
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

--  XXX to be up to date, we should complete implementation of
--   - exception framework
--   - CORBA.ORB.Shutdown
--   - POA.Destroy

--  $Id$

with Ada.Exceptions;
with Ada.Text_IO;

with GNAT.OS_Lib;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Report;

with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);
pragma Elaborate_All (PolyORB.Setup.Thread_Pool_Server);

with PolyORB.Tasking.Threads;

with CORBA.Impl;
with CORBA.Object;
with CORBA.ORB;
with CORBA.Policy;

with PortableServer.POA;
with PortableServer.POAManager;

with PortableServer.IdAssignmentPolicy;
with PortableServer.IdUniquenessPolicy;
with PortableServer.ImplicitActivationPolicy;
with PortableServer.LifespanPolicy;
with PortableServer.RequestProcessingPolicy;
with PortableServer.ServantRetentionPolicy;
with PortableServer.ThreadPolicy;

with Echo.Helper;
with Echo.Impl;

with Test_Job;

procedure Test000 is

   use Ada.Exceptions;
   use Ada.Text_IO;

   use CORBA;
   use PortableServer;
   use PortableServer.POA;
   use PortableServer.POAManager;

   use PortableServer.IdAssignmentPolicy;
   use PortableServer.IdUniquenessPolicy;
   use PortableServer.ImplicitActivationPolicy;
   use PortableServer.LifespanPolicy;
   use PortableServer.RequestProcessingPolicy;
   use PortableServer.ServantRetentionPolicy;
   use PortableServer.ThreadPolicy;

   use PolyORB.CORBA_P.Server_Tools;

   procedure Test_Init;
   --  Initialize test.

   procedure Attach_Servant (To_POA : PortableServer.POA.Ref;
                             Obj_Ref : out Echo.Ref);
   --  Attach an 'Echo' servant to 'To_POA' POA.

   procedure Invoke_On_Servant (Obj_Ref : Echo.Ref;
                                Reentrant : Boolean := False);
   --  Invoke on Servant 'Obj_Ref'.

   procedure Test_Root_POA;
   --  Test Root_POA.

   procedure Test_POAManager;
   --  Test POA Manager behavior.

   procedure Test_Single_Thread_Policy;
   --  Test POA Single_Thread Thread Policy.

   procedure Test_Main_Thread_Policy;
   --  Test POA Main_Thread Thread Policy.

   procedure Test_POA_Activation_Policies (POA : PortableServer.POA.Ref);
   --  Test Servant Activation Policies under POA's configuration.

   procedure Test_Conversion (POA : PortableServer.POA.Ref);
   --  Test Conversion functions under POA's configuration.

   function Create_POA_With_Policies
     (Tp : ThreadPolicyValue;
      Lp : LifespanPolicyValue;
      Up : IdUniquenessPolicyValue;
      Ap : IdAssignmentPolicyValue;
      Ip : ImplicitActivationPolicyValue;
      Sp : ServantRetentionPolicyValue;
      Rp : RequestProcessingPolicyValue)
     return PortableServer.POA.Ref;
   --  Regiter a Child POA of the RootPOA with the given policies.

   ---------------
   -- Test_Init --
   ---------------

   procedure Test_Init is
   begin
      --  ORB Initialization.
      CORBA.ORB.Initialize ("ORB");

      --  Run the ORB instance in a separated task.
      Initiate_Server (True);

      PolyORB.Report.Output ("ORB initialized", True);
   end Test_Init;

   --------------------
   -- Attach_Servant --
   --------------------

   procedure Attach_Servant (To_POA : PortableServer.POA.Ref;
                             Obj_Ref : out Echo.Ref)
   is
      Obj     : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;

   begin
      Obj_Ref := Echo.Helper.To_Ref
        (PortableServer.POA.Servant_To_Reference
         (To_POA,
          PortableServer.Servant (Obj)));

      PolyORB.Report.Output ("Attach servant to POA " &
                             To_Standard_String (Get_The_Name (To_POA)),
                             True);
   end Attach_Servant;

   -----------------------
   -- Invoke_On_Servant --
   -----------------------

   procedure Invoke_On_Servant (Obj_Ref : Echo.Ref;
                                Reentrant : Boolean := False) is
   begin
      if Reentrant then
         declare
            IOR : CORBA.String := CORBA.Object.Object_To_String (Obj_Ref);
         begin
            PolyORB.Report.Output
              ("Invocation on reentrant servant",
               IOR = Echo.echoString_reentrant (Obj_Ref, IOR));
         end;
      else
         PolyORB.Report.Output
           ("Invocation on created servant",
            "Hello Ada World !" =
            To_Standard_String
            (Echo.echoString
             (Obj_Ref, To_CORBA_String ("Hello Ada World !"))));
      end if;
   end Invoke_On_Servant;

   -------------------
   -- Test_Root_POA --
   -------------------

   procedure Test_Root_POA
   is
      Root_POA : PortableServer.POA.Ref;
      Obj_Ref  : Echo.Ref;
   begin
      PolyORB.Report.New_Test ("RootPOA");

      Root_POA := PortableServer.POA.To_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      PolyORB.Report.Output ("Get Root_POA reference", True);

      Attach_Servant (Root_POA, Obj_Ref);
      Invoke_On_Servant (Obj_Ref);

   end Test_Root_POA;

   ---------------------
   -- Test_POAManager --
   ---------------------

   procedure Test_POAManager
   is
      use CORBA.Policy.IDL_Sequence_Policy;

      Policies : CORBA.Policy.PolicyList;

      Thread_Policy : CORBA.Policy.Ref_Access
        := new CORBA.Policy.Ref'Class'(Create_Thread_Policy
                                       (PortableServer.ORB_CTRL_MODEL));

      Root_POA : constant PortableServer.POA.Ref :=
        PortableServer.POA.To_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      Child_POA : PortableServer.POA.Ref;

      Obj_Ref  : Echo.Ref;
   begin
      PolyORB.Report.New_Test ("POAManager");

      --  Construct POA Policy List.

      Append (Policies, Thread_Policy);

      --  Register a Child POA.

      Child_POA := PortableServer.POA.Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          To_CORBA_String ("Child_POA"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies));

      PolyORB.Report.Output ("Created child POA", True);

      --  Test invocation on a servant attached to this Child POA.

      Attach_Servant (Child_POA, Obj_Ref);
      Invoke_On_Servant (Obj_Ref);

      --  Now the POA will hold requests.

      Hold_Requests (PortableServer.POA.Get_The_POAManager (Child_POA),
                     False);
      PolyORB.Report.Output ("POA will hold requests, no request invocation",
                             True);

      Activate (PortableServer.POA.Get_The_POAManager (Child_POA));
      PolyORB.Report.Output ("POA is now active", True);

      --  Now the POA will hold requests, we also invoke a request.

      Hold_Requests (PortableServer.POA.Get_The_POAManager (Child_POA),
                     False);
      PolyORB.Report.Output ("POA will hold requests, request invocation",
                             True);

      Test_Job.Global_Obj_Ref := Obj_Ref;
      PolyORB.Tasking.Threads.Create_Task (Test_Job.Run_Job'Access);
      delay 1.0;
      --  Delay to provoke a context switch so that invocation
      --  actually begins before executing the next statement.

      PolyORB.Report.Output ("Invocation on servant began", True);

      Activate (PortableServer.POA.Get_The_POAManager (Child_POA));
      PolyORB.Report.Output ("POA is now active", True);

      --  Now the POA will discard requests.

      Discard_Requests (PortableServer.POA.Get_The_POAManager (Child_POA),
                        False);
      PolyORB.Report.Output ("POA will discard requests", True);

      begin
         Invoke_On_Servant (Obj_Ref);
      exception
         when others =>
            PolyORB.Report.Output ("Invoke raised exception", True);
            --  XXX should rework exception framework to rafine this test.
      end;

      Activate (PortableServer.POA.Get_The_POAManager (Child_POA));
      PolyORB.Report.Output ("POA has been reactived", True);
      Invoke_On_Servant (Obj_Ref);

      --  (dirty) Synchronization point.
      delay 5.0;
      PolyORB.Report.Output ("Waiting for end of POA Manager tests", True);

      Destroy (Child_POA, True, True);
      PolyORB.Report.Output ("POA has been destroyed", True);

   end Test_POAManager;

   -------------------------------
   -- Test_Single_Thread_Policy --
   -------------------------------

   procedure Test_Single_Thread_Policy
   is
      use CORBA.Policy.IDL_Sequence_Policy;

      Policies : CORBA.Policy.PolicyList;

      Thread_Policy : CORBA.Policy.Ref_Access
        := new CORBA.Policy.Ref'Class'(Create_Thread_Policy
                                       (PortableServer.SINGLE_THREAD_MODEL));

      Root_POA : constant PortableServer.POA.Ref :=
        PortableServer.POA.To_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      Child_POA : PortableServer.POA.Ref;
      Obj_Ref, Obj_Ref2  : Echo.Ref;
   begin
      PolyORB.Report.New_Test ("Single Thread Policy");

      --  Construct POA Policy List.
      Append (Policies, Thread_Policy);

      --  Register a Child POA.
      Child_POA := PortableServer.POA.Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          To_CORBA_String ("Child_POA"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies));

      PolyORB.Report.Output ("Created child POA with Single Thread policy",
                             True);

      --  Test the call is reentrant.

      --  XXX note that this test may or may not work, see
      --  PolyORB.POA_Policies.Thread_Policy.Single_Thread for more
      --  details.

      Attach_Servant (Child_POA, Obj_Ref);
      --      Invoke_On_Servant (Obj_Ref, True);

      --  Test multiple calls on the same servant.
      Test_Job.Global_Obj_Ref := Obj_Ref;
      PolyORB.Tasking.Threads.Create_Task (Test_Job.Run_Job_Wait'Access);
      delay 0.01;

      PolyORB.Tasking.Threads.Create_Task (Test_Job.Run_Job_Wait'Access);
      delay 0.01;
      --  Delay to provoke a context switch so that invocation
      --  actually begins before executing the next statement.

      --  (dirty) Synchronization point.
      delay 10.0;
      PolyORB.Report.Output ("Waiting for the end of multiple calls", True);

      Attach_Servant (Child_POA, Obj_Ref2);
      Invoke_On_Servant (Obj_Ref2);

      --  (dirty) Synchronization point.
      delay 10.0;
      PolyORB.Report.Output ("Waiting for end of Single Thread tests", True);

      Destroy (Child_POA, True, True);
      PolyORB.Report.Output ("POA has been destroyed", True);

   end Test_Single_Thread_Policy;

   -----------------------------
   -- Test_Main_Thread_Policy --
   -----------------------------

   procedure Test_Main_Thread_Policy
   is
      use CORBA.Policy.IDL_Sequence_Policy;

      Policies : CORBA.Policy.PolicyList;

      Thread_Policy : CORBA.Policy.Ref_Access
        := new CORBA.Policy.Ref'Class'(Create_Thread_Policy
                                       (PortableServer.MAIN_THREAD_MODEL));

      Root_POA : constant PortableServer.POA.Ref :=
        PortableServer.POA.To_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      Child_POA : PortableServer.POA.Ref;
      Obj_Ref, Obj_Ref2 : Echo.Ref;
   begin
      PolyORB.Report.New_Test ("Main Thread Policy");

      --  Construct POA Policy List.
      Append (Policies, Thread_Policy);

      --  Register a Child POA.
      Child_POA := PortableServer.POA.Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          To_CORBA_String ("Child_POA"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies));

      PolyORB.Report.Output ("Created child POA with Main Thread policy",
                             True);

      Attach_Servant (Child_POA, Obj_Ref);
      Invoke_On_Servant (Obj_Ref);

      --  Test multiple calls on the same servant.
      Test_Job.Global_Obj_Ref := Obj_Ref;
      PolyORB.Tasking.Threads.Create_Task (Test_Job.Run_Job_Wait'Access);
      delay 0.01;

      PolyORB.Tasking.Threads.Create_Task (Test_Job.Run_Job_Wait'Access);
      delay 0.01;
      --  Delay to provoke a context switch so that invocation
      --  actually begins before executing the next statement.

      Attach_Servant (Child_POA, Obj_Ref2);
      Invoke_On_Servant (Obj_Ref2);

      --  (dirty) Synchronization point.
      delay 10.0;
      PolyORB.Report.Output ("Waiting for end of Main Thread tests", True);

      Destroy (Child_POA, True, True);
      PolyORB.Report.Output ("POA has been destroyed", True);

   end Test_Main_Thread_Policy;

   ----------------------------------
   -- Test_POA_Activation_Policies --
   ----------------------------------

   procedure Test_POA_Activation_Policies
     (POA : PortableServer.POA.Ref) is
   begin
      PolyORB.Report.New_Test ("Activation Policies on POA "
                               & To_Standard_String (Get_The_Name (POA)));

      --  Servant_To_Refence implicitely activates servant.

      declare
         Servant : constant PortableServer.Servant := new Echo.Impl.Object;
         Obj_Ref : Echo.Ref;

      begin
         Obj_Ref := Echo.Helper.To_Ref
           (PortableServer.POA.Servant_To_Reference (POA, Servant));

         PolyORB.Report.Output ("Implicitly activate servant in POA "
                                & To_Standard_String (Get_The_Name (POA)),
                                True);

         Invoke_On_Servant (Obj_Ref);

         PolyORB.Report.Output
           ("Default Repository_Id is correct",
            Echo.Repository_Id = To_Standard_String
            (Get_Type_Id (Reference_To_Servant (POA, Obj_Ref))));
      end;

      --  Explicitely Activate servant.

      declare
         Servant : constant PortableServer.Servant := new Echo.Impl.Object;
         Obj_Ref : Echo.Ref;

         OID : ObjectId := PortableServer.POA.Activate_Object (POA, Servant);

      begin
         PolyORB.Report.Output ("Servant Activated", True);

         Obj_Ref := Echo.Helper.To_Ref
           (PortableServer.POA.Servant_To_Reference (POA, Servant));

         Invoke_On_Servant (Obj_Ref);

         PolyORB.Report.Output
           ("Default Repository_Id is correct",
            Echo.Repository_Id = To_Standard_String
            (Get_Type_Id (Reference_To_Servant (POA, Obj_Ref))));

         PortableServer.POA.Deactivate_Object (POA, OID);
         PolyORB.Report.Output ("Servant Deactivated", True);

         begin
            Invoke_On_Servant (Obj_Ref);
            PolyORB.Report.Output ("Exception raised on invocation", False);
         exception
            when others =>
               PolyORB.Report.Output ("Exception raised on invocation", True);
         end;
      end;

      --  Explicitely Activate servant with User Id.

      declare
         Servant : constant PortableServer.Servant := new Echo.Impl.Object;
         Obj_Ref : Echo.Ref;

         OID : ObjectId := PortableServer.String_To_ObjectId ("MyServant");

      begin
         begin
            PortableServer.POA.Activate_Object_With_Id
              (POA, OID, Servant);
            PolyORB.Report.Output ("Servant Activated With User Id", True);

            Obj_Ref := Echo.Helper.To_Ref
              (PortableServer.POA.Servant_To_Reference (POA, Servant));

            Invoke_On_Servant (Obj_Ref);

            PolyORB.Report.Output
              ("Default Repository_Id is correct",
               Echo.Repository_Id = To_Standard_String
               (Get_Type_Id (Reference_To_Servant (POA, Obj_Ref))));

            PortableServer.POA.Deactivate_Object (POA, OID);
            PolyORB.Report.Output ("Servant Deactivated", True);

            begin
               Invoke_On_Servant (Obj_Ref);
               PolyORB.Report.Output ("Exception raised on invocation", False);
            exception
               when others =>
                  PolyORB.Report.Output ("Exception raised on invocation",
                                         True);
            end;

         exception
            when others =>
               PolyORB.Report.Output ("Exception raised when Activating "
                                      & "servant with Id", True);
         end;
      end;

   end Test_POA_Activation_Policies;

   ----------------------
   -- Test_Conversion --
   ----------------------

   procedure Test_Conversion (POA : PortableServer.POA.Ref)
   is
      Servant : constant PortableServer.Servant := new Echo.Impl.Object;

   begin
      PolyORB.Report.New_Test ("Conversions");

      --  XXX these tests should test consistency between converted
      --  entities !!

      declare
         Obj_Ref : constant CORBA.Object.Ref
           := PortableServer.POA.Servant_To_Reference (POA, Servant);
      begin
         PolyORB.Report.Output ("Servant_To_Reference", True);

         declare
            OID : constant ObjectId := Reference_To_Id (POA, Obj_Ref);
         begin
            PolyORB.Report.Output ("Reference_To_Id", True);
         end;

         declare
            Servant : constant PortableServer.Servant
              := Reference_To_Servant (POA, Obj_Ref);
         begin
            PolyORB.Report.Output ("Reference_To_Servant", True);
         end;
      end;

      declare
         OID : constant ObjectId := Servant_To_Id (POA, Servant);
      begin
         PolyORB.Report.Output ("Servant_To_Id", True);

         declare
            Servant : constant PortableServer.Servant
              := Id_To_Servant (POA, OID);
         begin
            PolyORB.Report.Output ("Id_To_Servant", True);
         end;

         declare
            Obj_Ref : constant CORBA.Object.Ref
              := Id_To_Reference (POA, OID);
         begin
            PolyORB.Report.Output ("Id_To_Reference", True);
         end;
      end;
   end Test_Conversion;

   ------------------------------
   -- Create_POA_With_Policies --
   ------------------------------

   function Create_POA_With_Policies
     (Tp : ThreadPolicyValue;
      Lp : LifespanPolicyValue;
      Up : IdUniquenessPolicyValue;
      Ap : IdAssignmentPolicyValue;
      Ip : ImplicitActivationPolicyValue;
      Sp : ServantRetentionPolicyValue;
      Rp : RequestProcessingPolicyValue)
     return PortableServer.POA.Ref
   is
      use CORBA.Policy.IDL_Sequence_Policy;

      Policies : CORBA.Policy.PolicyList;

      Thread_Policy : CORBA.Policy.Ref_Access
        := new CORBA.Policy.Ref'Class'(Create_Thread_Policy (Tp));

      Lifespan_Policy : CORBA.Policy.Ref_Access
        := new CORBA.Policy.Ref'Class'(Create_Lifespan_Policy (Lp));

      Id_Uniqueness_Policy : CORBA.Policy.Ref_Access
        := new CORBA.Policy.Ref'Class'(Create_Id_Uniqueness_Policy (Up));

      Id_Assignment_Policy : CORBA.Policy.Ref_Access
        := new CORBA.Policy.Ref'Class'(Create_Id_Assignment_Policy (Ap));

      Implicit_Activation_Policy : CORBA.Policy.Ref_Access
        := new CORBA.Policy.Ref'Class'(Create_Implicit_Activation_Policy (Ip));

      Servant_Retention_Policy : CORBA.Policy.Ref_Access
        := new CORBA.Policy.Ref'Class'(Create_Servant_Retention_Policy (Sp));

      Request_Processing_Policy : CORBA.Policy.Ref_Access
        := new CORBA.Policy.Ref'Class'(Create_Request_Processing_Policy (Rp));

      Root_POA : constant PortableServer.POA.Ref :=
        PortableServer.POA.To_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      Child_POA : PortableServer.POA.Ref;
   begin

      Append (Policies, Thread_Policy);
      Append (Policies, Lifespan_Policy);
      Append (Policies, Id_Uniqueness_Policy);
      Append (Policies, Id_Assignment_Policy);
      Append (Policies, Implicit_Activation_Policy);
      Append (Policies, Servant_Retention_Policy);
      Append (Policies, Request_Processing_Policy);

      Child_POA := PortableServer.POA.Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          To_CORBA_String ("Child_POA"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies));

      return Child_POA;
   end Create_POA_With_Policies;

begin
   Test_Init;
   Test_Root_POA;
   Test_POAManager;
   Test_Single_Thread_Policy;
   Test_Main_Thread_Policy;
   Test_POA_Activation_Policies (Get_Root_POA);
   Test_Conversion (Get_Root_POA);

   PolyORB.Report.End_Report;

   GNAT.OS_Lib.OS_Exit (1);
   --  XXX Work around to dirty exit, to be removed when
   --  CORBA.ORB.Shutdown is implemented ...

exception
   when E : others =>
      Put_Line ("Got exception "
                & Exception_Name (E)
                & " : "
                & Exception_Message (E));
      PolyORB.Report.Output ("END TESTS", False);
      GNAT.OS_Lib.OS_Exit (1);

end Test000;
