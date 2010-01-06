------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        T E S T 0 0 0 _ S E T U P                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2008, Free Software Foundation, Inc.          --
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

--  XXX should test POA self destruction

with Ada.Exceptions;
with Ada.Text_IO;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Utils.Report;

with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);

with PolyORB.Tasking.Threads;

with CORBA.Impl;
with CORBA.Object;
with CORBA.ORB;
with CORBA.Policy;

with PortableServer.POA.Helper;
with PortableServer.POAManager;

with Echo.Helper;
with Echo.Impl;

with Test_Job;

package body Test000_Setup is

   use Ada.Text_IO;
   use Ada.Exceptions;

   use CORBA;
   use PortableServer.POAManager;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Log;
   use PolyORB.Utils.Report;

   package L is new PolyORB.Log.Facility_Log ("test000");
   procedure O (Message : Standard.String;
                Level : PolyORB.Log.Log_Level := PolyORB.Log.Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   ---------------
   -- Init_Test --
   ---------------

   procedure Init_Test is
   begin
      --  ORB Initialization.

      CORBA.ORB.Initialize ("ORB");

      --  Run the ORB instance in a separated task.

      Initiate_Server (True);

      Output ("ORB initialized", True);
   end Init_Test;

   --------------------
   -- Attach_Servant --
   --------------------

   procedure Attach_Servant
     (To_POA  : PortableServer.POA.Local_Ref;
      Obj_Ref : out Echo.Ref)
   is
      Obj : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;

   begin
      Obj_Ref := Echo.Helper.To_Ref
        (PortableServer.POA.Servant_To_Reference
         (To_POA, PortableServer.Servant (Obj)));

      Output ("Attach servant to POA "
              & To_Standard_String (Get_The_Name (To_POA)),
              True);
   end Attach_Servant;

   -----------------------
   -- Invoke_On_Servant --
   -----------------------

   procedure Invoke_On_Servant
     (Obj_Ref   : Echo.Ref;
      Reentrant : Boolean := False;
      Verbose   : Boolean := True) is
   begin
      if Reentrant then
         declare
            IOR : constant CORBA.String
              := CORBA.Object.Object_To_String (Obj_Ref);
         begin
            if Verbose then
               Output
                 ("Invocation on reentrant servant",
                  IOR = Echo.echoString_reentrant (Obj_Ref, IOR));
            end if;
         end;
      else
         if Verbose then
            Output
              ("Invocation on created servant",
               "Hello Ada World !" =
               To_Standard_String
               (Echo.echoString
                (Obj_Ref, To_CORBA_String ("Hello Ada World !"))));
         end if;
      end if;
   end Invoke_On_Servant;

   function Invoke_On_Servant
     (Obj_Ref   : Echo.Ref)
     return Boolean is
   begin
      return "Hello Ada World !" =
        To_Standard_String
        (Echo.echoString
         (Obj_Ref, To_CORBA_String ("Hello Ada World !")));
   end Invoke_On_Servant;

   -------------------
   -- Test_Root_POA --
   -------------------

   procedure Test_Root_POA
   is
      Root_POA : PortableServer.POA.Local_Ref;
      Obj_Ref  : Echo.Ref;
   begin
      New_Test ("RootPOA");

      Root_POA := PortableServer.POA.Helper.To_Local_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      Output ("Get Root_POA reference", True);

      Attach_Servant (Root_POA, Obj_Ref);
      Invoke_On_Servant (Obj_Ref);
   end Test_Root_POA;

   ---------------------
   -- Test_POAManager --
   ---------------------

   procedure Test_POAManager
   is
      use CORBA.Policy.IDL_SEQUENCE_Policy;

      Policies : CORBA.Policy.PolicyList;

      Thread_Policy : constant CORBA.Policy.Ref :=
        CORBA.Policy.Ref (Create_Thread_Policy
                          (PortableServer.ORB_CTRL_MODEL));

      Root_POA : constant PortableServer.POA.Local_Ref :=
        PortableServer.POA.Helper.To_Local_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      Child_POA_Manager : PortableServer.POAManager.Local_Ref;

      Child_POA : PortableServer.POA.Local_Ref;

      Obj_Ref  : Echo.Ref;
   begin
      New_Test ("POAManager");

      --  Construct POA Policy List.

      Append (Policies, Thread_Policy);

      --  Register a Child POA.

      Child_POA := PortableServer.POA.Local_Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          To_CORBA_String ("Child_POA"),
          Child_POA_Manager,
          Policies));

      Output ("Created child POA", True);

      Activate (PortableServer.POA.Get_The_POAManager (Child_POA));
      Output ("POA is now active", True);

      --  Test invocation on a servant attached to this Child POA.

      Attach_Servant (Child_POA, Obj_Ref);
      Invoke_On_Servant (Obj_Ref);

      --  Now the POAManager will hold requests.

      Hold_Requests (PortableServer.POA.Get_The_POAManager (Child_POA),
                     False);
      Output ("POA will hold requests, no request invocation",
                             True);

      Activate (PortableServer.POA.Get_The_POAManager (Child_POA));
      Output ("POA is now active", True);

      --  POAManager state sanity check

      Output ("POA State Correct ",
              Get_State (PortableServer.POA.Get_The_POAManager (Child_POA)) =
              PortableServer.POAManager.ACTIVE);

      --  Now the POAManager will hold requests, we also invoke a request
      --  to test POAManager queue.

      Hold_Requests (PortableServer.POA.Get_The_POAManager (Child_POA),
                     False);
      Output ("POA will hold requests, request invocation",
                             True);

      --  POAManager state sanity check

      Output ("POA State Correct ",
              Get_State (PortableServer.POA.Get_The_POAManager (Child_POA))
              = PortableServer.POAManager.HOLDING);

      Test_Job.Global_Obj_Ref := Obj_Ref;
      PolyORB.Tasking.Threads.Create_Task (Test_Job.Run_Job'Access);

      delay 0.1;
      --  Delay to provoke a context switch so that invocation
      --  actually begins before executing the next statement.

      Output ("Invocation on servant began", True);

      Activate (PortableServer.POA.Get_The_POAManager (Child_POA));
      Output ("POA is now active", True);

      --  POAManager state sanity check

      Output ("POA State Correct ",
              Get_State (PortableServer.POA.Get_The_POAManager (Child_POA)) =
              PortableServer.POAManager.ACTIVE);

      --  (dirty) Synchronization point.

      delay 5.0;
      Output ("Waiting for end of POAManager 'HOLDING' tests", True);

      --  Now the POAManager will discard requests.

      Discard_Requests (PortableServer.POA.Get_The_POAManager (Child_POA),
                        False);

      --  POAManager state sanity check

      Output ("POA State Correct ",
              Get_State (PortableServer.POA.Get_The_POAManager (Child_POA)) =
              PortableServer.POAManager.DISCARDING);

      Output ("POA will discard requests", True);

      begin
         Invoke_On_Servant (Obj_Ref);
         Output ("Invoke raised exception", False);
      exception
         when CORBA.Transient =>
            Output ("Invoke raised exception", True);

         when E : others =>
            Put_Line ("Got exception "
                      & Exception_Name (E)
                      & " : "
                      & Exception_Message (E));
            raise;
      end;

      Activate (PortableServer.POA.Get_The_POAManager (Child_POA));
      Output ("POA has been reactived", True);

      --  POAManager state sanity check

      Output ("POA State Correct ",
              Get_State (PortableServer.POA.Get_The_POAManager (Child_POA)) =
              PortableServer.POAManager.ACTIVE);

      Invoke_On_Servant (Obj_Ref);

      --  Now the POAManager is deactivated

      Deactivate (PortableServer.POA.Get_The_POAManager (Child_POA),
                  False,
                  False);

      --  POAManager state sanity check

      Output ("POA State Correct ",
              Get_State (PortableServer.POA.Get_The_POAManager (Child_POA)) =
              PortableServer.POAManager.INACTIVE);

      begin
         Activate (PortableServer.POA.Get_The_POAManager (Child_POA));
         Output ("Activate raised exception", False);

      exception
         when PortableServer.POAManager.AdapterInactive =>
            Output ("Activate raised exception", True);

         when E : others =>
            Put_Line ("Got exception "
                      & Exception_Name (E)
                      & " : "
                      & Exception_Message (E));
            raise;
      end;

      Destroy (Child_POA, True, True);
      Output ("POA has been destroyed", True);

   end Test_POAManager;

   -------------------------------
   -- Test_Single_Thread_Policy --
   -------------------------------

   procedure Test_Single_Thread_Policy
   is
      use CORBA.Policy.IDL_SEQUENCE_Policy;

      Policies : CORBA.Policy.PolicyList;

      Thread_Policy : constant CORBA.Policy.Ref :=
        CORBA.Policy.Ref (Create_Thread_Policy
                          (PortableServer.SINGLE_THREAD_MODEL));

      Root_POA : constant PortableServer.POA.Local_Ref :=
        PortableServer.POA.Helper.To_Local_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      Child_POA : PortableServer.POA.Local_Ref;
      Obj_Ref, Obj_Ref2  : Echo.Ref;
   begin
      New_Test ("Single Thread Policy");

      --  Construct POA Policy List.

      Append (Policies, Thread_Policy);

      --  Register a Child POA.

      Child_POA := PortableServer.POA.Local_Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          To_CORBA_String ("Child_POA"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies));

      Output ("Created child POA with Single Thread policy",
                             True);

      --  Test the call is reentrant.

      --  XXX note that this test may or may not work, see
      --  PolyORB.POA_Policies.Thread_Policy.Single_Thread for more
      --  details.

      Attach_Servant (Child_POA, Obj_Ref);
      --  Invoke_On_Servant (Obj_Ref, True);

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
      Output ("Waiting for the end of multiple calls", True);

      Attach_Servant (Child_POA, Obj_Ref2);
      Invoke_On_Servant (Obj_Ref2);

      --  (dirty) Synchronization point.

      delay 10.0;
      Output ("Waiting for end of Single Thread tests", True);

      Destroy (Child_POA, True, True);
      Output ("POA has been destroyed", True);

   end Test_Single_Thread_Policy;

   -----------------------------
   -- Test_Main_Thread_Policy --
   -----------------------------

   procedure Test_Main_Thread_Policy
   is
      use CORBA.Policy.IDL_SEQUENCE_Policy;

      Policies : CORBA.Policy.PolicyList;

      Thread_Policy : constant CORBA.Policy.Ref :=
        CORBA.Policy.Ref (Create_Thread_Policy
                          (PortableServer.MAIN_THREAD_MODEL));

      Root_POA : constant PortableServer.POA.Local_Ref :=
        PortableServer.POA.Helper.To_Local_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      Child_POA : PortableServer.POA.Local_Ref;
      Obj_Ref, Obj_Ref2 : Echo.Ref;
   begin
      New_Test ("Main Thread Policy");

      --  Construct POA Policy List.

      Append (Policies, Thread_Policy);

      --  Register a Child POA.

      Child_POA := PortableServer.POA.Local_Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          To_CORBA_String ("Child_POA"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies));

      Output ("Created child POA with Main Thread policy",
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
      Output ("Waiting for end of Main Thread tests", True);

      Destroy (Child_POA, True, True);
      Output ("POA has been destroyed", True);

   end Test_Main_Thread_Policy;

   ---------------------
   -- Test_Conversion --
   ---------------------

   procedure Test_Conversion (POA : PortableServer.POA.Local_Ref)
   is
      Servant : constant PortableServer.Servant := new Echo.Impl.Object;

   begin
      New_Test ("Conversions");

      --  XXX these tests should test consistency between converted
      --  entities !!

      declare
         Obj_Ref : constant CORBA.Object.Ref :=
           PortableServer.POA.Servant_To_Reference (POA, Servant);
      begin
         Output ("Servant_To_Reference", True);

         declare
            Oid : constant ObjectId := Reference_To_Id (POA, Obj_Ref);
            pragma Unreferenced (Oid);
         begin
            Output ("Reference_To_Id", True);
         end;

         declare
            Servant : constant PortableServer.Servant :=
              Reference_To_Servant (POA, Obj_Ref);
            pragma Unreferenced (Servant);
         begin
            Output ("Reference_To_Servant", True);
         end;
      end;

      declare
         Oid : constant ObjectId := Servant_To_Id (POA, Servant);
      begin
         Output ("Servant_To_Id", True);

         declare
            Servant : constant PortableServer.Servant :=
              Id_To_Servant (POA, Oid);
            pragma Unreferenced (Servant);
         begin
            Output ("Id_To_Servant", True);
         end;

         declare
            Obj_Ref : constant CORBA.Object.Ref :=
              Id_To_Reference (POA, Oid);
            pragma Unreferenced (Obj_Ref);
         begin
            Output ("Id_To_Reference", True);
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
     return PortableServer.POA.Local_Ref
   is
      use CORBA.Policy.IDL_SEQUENCE_Policy;

      Policies : CORBA.Policy.PolicyList;

      Thread_Policy : constant CORBA.Policy.Ref :=
        CORBA.Policy.Ref
        (Create_Thread_Policy (Tp));

      Lifespan_Policy : constant CORBA.Policy.Ref :=
        CORBA.Policy.Ref
        (Create_Lifespan_Policy (Lp));

      Id_Uniqueness_Policy : constant CORBA.Policy.Ref :=
        CORBA.Policy.Ref
        (Create_Id_Uniqueness_Policy (Up));

      Id_Assignment_Policy : constant CORBA.Policy.Ref :=
        CORBA.Policy.Ref
        (Create_Id_Assignment_Policy (Ap));

      Implicit_Activation_Policy : constant CORBA.Policy.Ref :=
        CORBA.Policy.Ref
        (Create_Implicit_Activation_Policy (Ip));

      Servant_Retention_Policy : constant CORBA.Policy.Ref :=
        CORBA.Policy.Ref
        (Create_Servant_Retention_Policy (Sp));

      Request_Processing_Policy : constant CORBA.Policy.Ref :=
        CORBA.Policy.Ref
        (Create_Request_Processing_Policy (Rp));

      Root_POA : constant PortableServer.POA.Local_Ref :=
        PortableServer.POA.Helper.To_Local_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      Child_POA : PortableServer.POA.Local_Ref;
   begin

      Append (Policies, Thread_Policy);
      Append (Policies, Lifespan_Policy);
      Append (Policies, Id_Uniqueness_Policy);
      Append (Policies, Id_Assignment_Policy);
      Append (Policies, Implicit_Activation_Policy);
      Append (Policies, Servant_Retention_Policy);
      Append (Policies, Request_Processing_Policy);

      Child_POA := PortableServer.POA.Local_Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          To_CORBA_String ("Child_POA"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies));

      return Child_POA;
   end Create_POA_With_Policies;

   --------------------
   -- Policies_Image --
   --------------------

   function Policies_Image
     (Tp : ThreadPolicyValue;
      Lp : LifespanPolicyValue;
      Up : IdUniquenessPolicyValue;
      Ap : IdAssignmentPolicyValue;
      Ip : ImplicitActivationPolicyValue;
      Sp : ServantRetentionPolicyValue;
      Rp : RequestProcessingPolicyValue)
     return String is
   begin
      return " "
        & Tp'Img & " " & Lp'Img & " " & Up'Img & " "
        & Ap'Img & " " & Ip'Img & " " & Sp'Img & " " & Rp'Img & " ";
   end Policies_Image;

   ------------------------
   -- Are_Policies_Valid --
   ------------------------

   function Are_Policies_Valid
     (Tp : ThreadPolicyValue;
      Lp : LifespanPolicyValue;
      Up : IdUniquenessPolicyValue;
      Ap : IdAssignmentPolicyValue;
      Ip : ImplicitActivationPolicyValue;
      Sp : ServantRetentionPolicyValue;
      Rp : RequestProcessingPolicyValue)
     return Boolean
   is
      pragma Unreferenced (Tp, Lp);
   begin
      if (Up = UNIQUE_ID and then Sp = NON_RETAIN)
        or else (Sp = NON_RETAIN
                 and then not (Rp = USE_DEFAULT_SERVANT
                               or else Rp = USE_SERVANT_MANAGER))
        or else (Ip = IMPLICIT_ACTIVATION
                 and then not (Ap = SYSTEM_ID and then Sp = RETAIN))
        or else (Rp = USE_ACTIVE_OBJECT_MAP_ONLY and then Sp /= RETAIN)
        or else (Rp = USE_DEFAULT_SERVANT and then Up /= MULTIPLE_ID)
      then
         return False;
      else
         return True;
      end if;
   end Are_Policies_Valid;

   ----------------------------
   -- Create_And_Destroy_POA --
   ----------------------------

   function Create_And_Destroy_POA
     (Tp : ThreadPolicyValue;
      Lp : LifespanPolicyValue;
      Up : IdUniquenessPolicyValue;
      Ap : IdAssignmentPolicyValue;
      Ip : ImplicitActivationPolicyValue;
      Sp : ServantRetentionPolicyValue;
      Rp : RequestProcessingPolicyValue)
     return Boolean
   is
      Test_POA : PortableServer.POA.Local_Ref;
   begin
      Test_POA := Create_POA_With_Policies
        (Tp, Lp, Up, Ap, Ip, Sp, Rp);

      PortableServer.POA.Destroy
        (Test_POA, False, False);

      return Are_Policies_Valid (Tp, Lp, Up, Ap, Ip, Sp, Rp);

   exception
      when E : PortableServer.POA.InvalidPolicy =>
         if Are_Policies_Valid (Tp, Lp, Up, Ap, Ip, Sp, Rp) then
            --  If policies are valid, then there is a problem.

            New_Line;
            Put_Line ("Got exception "
                      & Exception_Name (E)
                      & " : "
                      & Exception_Message (E));
            Put_Line ("Valid ? "
                      & Boolean'Image
                      (Are_Policies_Valid (Tp, Lp, Up, Ap, Ip, Sp, Rp)));
            Put_Line (Policies_Image (Tp, Lp, Up, Ap, Ip, Sp, Rp));
         end if;

         return not Are_Policies_Valid (Tp, Lp, Up, Ap, Ip, Sp, Rp);

      when E : others =>
         Output ("Fatal Error, got exception ", False);
         Put_Line (Ada.Exceptions.Exception_Information (E));
         return False;
   end Create_And_Destroy_POA;

   -----------------------
   -- Test_POA_Creation --
   -----------------------

   procedure Test_POA_Creation is
      Result : Boolean := True;

   begin
      New_Test ("POA Creation");

      for Tp in ThreadPolicyValue'Range loop
         for Lp in LifespanPolicyValue'Range loop
            for Up in IdUniquenessPolicyValue'Range loop
               for Ap in IdAssignmentPolicyValue'Range loop
                  for Ip in ImplicitActivationPolicyValue'Range loop
                     for Sp in ServantRetentionPolicyValue'Range loop
                        for Rp in RequestProcessingPolicyValue'Range loop

                           pragma Debug (O (" "));
                           pragma Debug (O ("Testing: "
                                            & Policies_Image
                                            (Tp, Lp, Up, Ap, Ip, Sp, Rp)));

                           Result := Result and Create_And_Destroy_POA
                             (Tp, Lp, Up, Ap, Ip, Sp, Rp);

                           if not Result then
                              Put_Line
                                (Policies_Image (Tp, Lp, Up, Ap, Ip, Sp, Rp));
                              exit;
                           end if;

                        end loop;
                     end loop;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;

      Output ("Test_POA_Creation", Result);
   end Test_POA_Creation;

   ----------------------------------
   -- Test_POA_Activation_Policies --
   ----------------------------------

   type Result_Vector is record
      Implicit_Activation     : Boolean := True;
      Get_Type_Id             : Boolean := True;
      Activation_No_Id        : Boolean := True;
      Unique_Activation_No_Id : Boolean := True;
      Deactivation_No_Id      : Boolean := True;
      Activation_Id           : Boolean := True;
      Unique_Activation_Id    : Boolean := True;
      Deactivation_Id         : Boolean := True;
      Default_Servant_No_Id   : Boolean := True;
      Default_Servant_Id      : Boolean := True;

      Fatal                   : Boolean := False;
   end record;

   function Test_POA_Activation_Policies
     (POA : PortableServer.POA.Local_Ref) return Result_Vector;
   --  Test Servant Activation Policies under POA's configuration.

   function Test_POA_Activation_Policies
     (POA : PortableServer.POA.Local_Ref) return Result_Vector
   is
      Result : Result_Vector;
      Temp   : Boolean;
   begin

      --
      --  Servant_To_Refence implicitly activates servant.
      --

      pragma Debug (O ("  ==> Implicit Activation sub test <=="));

      declare
         Servant : constant PortableServer.Servant := new Echo.Impl.Object;
         Obj_Ref : Echo.Ref;

      begin

         --  Call Servant_To_Reference.

         begin
            Obj_Ref := Echo.Helper.To_Ref
              (PortableServer.POA.Servant_To_Reference (POA, Servant));
         exception
            when PortableServer.POA.WrongPolicy  =>
               pragma Debug (O ("Servant_To_Reference failed on WrongPolicy"));
               Result.Implicit_Activation := False;

            when PortableServer.POA.ServantNotActive =>
               pragma Debug (O ("Servant_To_Reference failed on "
                                & "ServantNotActive"));
               Result.Implicit_Activation := False;

            when E : others =>
               pragma Debug (O ("Got exception "
                                & Exception_Name (E)
                                & " : "
                                & Exception_Message (E)));
               Result.Fatal := True;
         end;

         --  Try to Invoke on the Reference we built.

         begin
            Temp := Invoke_On_Servant (Obj_Ref);

            if not Temp then
               pragma Debug (O ("FATAL: invocation failed"));
               Result.Fatal := True;
            end if;

         exception
            when CORBA.Inv_Objref =>
               pragma Debug (O ("Implicit_Activation: Invoke failed"));
               Result.Implicit_Activation := False;

            when E : others =>
               pragma Debug (O ("Got exception "
                                & Exception_Name (E)
                                & " : "
                                & Exception_Message (E)));
               Result.Fatal := True;
         end;

      exception
         when E : others =>
            pragma Debug (O ("FATAL: Got exception @0 " & Exception_Name (E)));
            Result.Fatal := True;
      end;

      --
      --  Explicitly Activate servant with No Id.
      --

      pragma Debug (O ("  ==> Explicit Activation System Id sub test <=="));

      declare
         Servant : constant PortableServer.Servant := new Echo.Impl.Object;
         Obj_Ref : Echo.Ref;
      begin

         declare

            --  Explicit Object Activation with no supplied Id

            Oid : constant ObjectId :=
              PortableServer.POA.Activate_Object (POA, Servant);

         begin

            --  Call Servant_To_Reference

            Obj_Ref := Echo.Helper.To_Ref
              (PortableServer.POA.Servant_To_Reference (POA, Servant));

            --  Try to invoke on the Object Ref we created from the Servant

            Temp := Invoke_On_Servant (Obj_Ref);

            if not Temp then
               pragma Debug (O ("FATAL: Invoke_On_Servant failed"));
               Result.Fatal := True;
            end if;

            --  Repository Id sanity check

            Temp := Echo.Repository_Id
              = Get_Type_Id (Reference_To_Servant (POA, Obj_Ref));

            if not Temp then
               pragma Debug (O ("Get_Type_Id failed"));
               Result.Get_Type_Id := False;
            end if;

            --  Activate twice the same Object

            begin
               declare
                  Oid2 : constant ObjectId :=
                    PortableServer.POA.Activate_Object (POA, Servant);
               begin
                  Result.Unique_Activation_No_Id := False;

                  --  Try to invoke on the Servant with new Oid

                  declare
                     Obj_Ref2 : constant Echo.Ref :=
                       Echo.Helper.To_Ref (Id_To_Reference (POA, Oid2));
                  begin
                     Temp := Invoke_On_Servant (Obj_Ref2);

                     if not Temp then
                        pragma Debug (O ("FATAL: Invoke_On_Servant failed"));
                        Result.Fatal := True;
                     end if;
                  end;
               end;

            exception
               when PortableServer.POA.ServantAlreadyActive =>
                  null;
            end;

            --  Deactivate Object

            begin
               PortableServer.POA.Deactivate_Object (POA, Oid);
            exception
               when E : others =>
                  pragma Debug (O ("Got exception "
                                   & Exception_Name (E)
                                   & " : "
                                   & Exception_Message (E)));

                  pragma Debug (O ("Deactivation_No_Id failed"));
                  Result.Deactivation_No_Id := False;
            end;

            begin
               Temp := Invoke_On_Servant (Obj_Ref);
               pragma Debug (O ("FATAL: Did an invocation on a 'should-not-be'"
                                & " activated servant !!"));
               Result.Fatal := True;
            exception
               when others =>
                  null;
            end;

            --  Activate Object With Id we get from the POA

            begin
               PortableServer.POA.Activate_Object_With_Id (POA, Oid, Servant);
            exception
               when others =>
                  pragma Debug (O ("FATAL: Reactivation with Id failed"));
                  Result.Fatal := True;
            end;

            begin
               Temp := Invoke_On_Servant (Obj_Ref);

            exception
               when E : others =>
                  pragma Debug (O ("Got exception "
                                   & Exception_Name (E)
                                   & " : "
                                   & Exception_Message (E)));
                  pragma Debug (O ("FATAL: Invoke_On_Servant "
                                   & "raised an exception"));
                  Result.Fatal := True;
            end;

            --  Deactivate Object

            begin
               PortableServer.POA.Deactivate_Object (POA, Oid);
            exception
               when E : others =>
                  pragma Debug (O ("Got exception "
                                   & Exception_Name (E)
                                   & " : "
                                   & Exception_Message (E)));

                  pragma Debug (O ("Deactivation_No_Id failed"));
                  Result.Deactivation_No_Id := False;
            end;

            begin
               Temp := Invoke_On_Servant (Obj_Ref);
               pragma Debug (O ("FATAL: Did an invocation on a 'should-not-be'"
                                & " activated servant !!"));
               Result.Fatal := True;
            exception
               when others =>
                  null;
            end;

         end;

      exception
         when E : others =>
            pragma Debug (O ("Got exception "
                             & Exception_Name (E)
                             & " : "
                             & Exception_Message (E)));

            pragma Debug (O ("Activation_No_Id failed"));
            Result.Activation_No_Id := False;

            pragma Debug (O ("Reactivation_Id failed"));

            pragma Debug (O ("Deactivation_No_Id failed"));
            Result.Deactivation_No_Id := False;

            Result.Unique_Activation_No_Id := False;
      end;

      --
      --  Explicitly Activate servant with User Id.
      --

      pragma Debug (O ("  ==> Explicit Activation User Id sub test <=="));

      declare
         Servant : constant PortableServer.Servant := new Echo.Impl.Object;
         Obj_Ref : Echo.Ref;

         Oid : constant ObjectId :=
           PortableServer.String_To_ObjectId ("dead");

      begin
         --  Explicit Object Activation with User supplied Object_Id.

         PortableServer.POA.Activate_Object_With_Id
           (POA, Oid, Servant);

         --  Repository Id sanity check

         Temp := Echo.Repository_Id =
           Get_Type_Id (Reference_To_Servant (POA, Obj_Ref));

         if not Temp then
            pragma Debug (O ("Get_Type_Id failed"));
            Result.Get_Type_Id := False;
         end if;

         --  Test Servant to Id integrity.

         declare
            Oid2 : constant ObjectId :=
              PortableServer.POA.Servant_To_Id (POA, Servant);
         begin
            if Oid /= Oid2 then
               Result.Fatal := True;
               pragma Debug (O (PortableServer.ObjectId_To_String (Oid)));
               pragma Debug (O (PortableServer.ObjectId_To_String (Oid2)));
               --  Having Oid /= Oid2 is valid when using MULTIPLE_ID
            end if;
         end;

         --  Call Servant_To_Reference.

         Obj_Ref := Echo.Helper.To_Ref
           (PortableServer.POA.Servant_To_Reference (POA, Servant));

         --  Try to invoke on the Ref we created from the Servant

         Temp := Invoke_On_Servant (Obj_Ref);

         if not Temp then
            pragma Debug (O ("FATAL: Invoke_On_Servant failed"));
            Result.Fatal := True;
         end if;

         --  Deactivate Object

         begin
            PortableServer.POA.Deactivate_Object (POA, Oid);

         exception
            when E : others =>
               pragma Debug (O ("Got exception "
                                & Exception_Name (E)
                                & " : "
                                & Exception_Message (E)));
               pragma Debug (O ("Deactivaion_Id failed"));
               Result.Deactivation_Id := False;
         end;

         begin
            Temp := Invoke_On_Servant (Obj_Ref);

            pragma Debug (O ("FATAL: Invoke_On_Servant raised no exception"));
            Result.Fatal := True;
         exception
            when others =>
               null;
         end;

         --  Try to invoke on a Ref created from the User's Oid

         declare
            Obj_Ref2 : constant Echo.Ref :=
              Echo.Helper.To_Ref
              (Create_Reference_With_Id
               (POA,
                Oid,
                To_CORBA_String (Echo.Repository_Id)));

            Temp_Oid : constant PortableServer.ObjectId :=
              PortableServer.POA.Activate_Object
              (POA, Reference_To_Servant (POA, Obj_Ref2));
            pragma Unreferenced (Temp_Oid);

         begin
            --  Repository Id sanity check

            Temp := Echo.Repository_Id =
              Get_Type_Id (Reference_To_Servant (POA, Obj_Ref2));

            if not Temp then
               pragma Debug (O ("Get_Type_Id failed"));
               Result.Get_Type_Id := False;
            end if;

            Temp := Invoke_On_Servant (Obj_Ref2);

         exception
            when E : others =>
               pragma Debug (O ("Got exception "
                                & Exception_Name (E)
                                & " : "
                                & Exception_Message (E)));
               pragma Debug (O ("FATAL: Invoke_On_Servant "
                                & "raised an exception"));
               Result.Fatal := True;
         end;

         --  Activate twice the same Object with the same Id.

         begin
            PortableServer.POA.Activate_Object_With_Id (POA, Oid, Servant);
            PortableServer.POA.Activate_Object_With_Id (POA, Oid, Servant);
            Result.Unique_Activation_Id := False;
         exception
            when PortableServer.POA.ServantAlreadyActive =>
               null;
         end;

      exception
         when E : others =>
            pragma Debug (O ("Got exception "
                             & Exception_Name (E)
                             & " "
                             & Exception_Message (E)));

            pragma Debug (O ("Activation_Id failed"));
            Result.Activation_Id := False;

            pragma Debug (O ("Deactivaion_Id failed"));
            Result.Deactivation_Id := False;

            Result.Unique_Activation_Id := False;
      end;

      --
      --  Default servant tests
      --

      pragma Debug (O ("  ==> Default servant test <=="));

      declare
         Servant : constant PortableServer.Servant := new Echo.Impl.Object;
         Temp : Boolean;
      begin

         --  Default Servant should be null

         begin
            declare
               Servant2 : constant PortableServer.Servant := Get_Servant (POA);
               pragma Unreferenced (Servant2);
            begin
               pragma Debug (O ("FATAL: Get Servant raised no exception"));
               Result.Fatal := True;
            end;
         exception
            when PortableServer.POA.NoServant =>
               null;

         end;

         --  Test Set_Sevant

         begin
            Set_Servant (POA, Servant);
         exception
            when PortableServer.POA.WrongPolicy =>
               raise;

            when E : others =>
               pragma Debug (O ("Got exception A"
                                & Exception_Name (E)
                                & " "
                                & Exception_Message (E)));
               Result.Fatal := True;
         end;

         --  Sanity check

         Temp := Servant = Get_Servant (POA);

         if not Temp then
            pragma Debug (O ("FATAL: Default Servant check failed"));
            Result.Fatal := True;
         end if;

         --  Test invocation on default servant with System Id

         begin
            declare
               Obj_Ref2 : constant Echo.Ref :=
                 Echo.Helper.To_Ref
                 (Create_Reference
                  (POA,
                   To_CORBA_String (Echo.Repository_Id)));
            begin
               Temp := Invoke_On_Servant (Obj_Ref2);

               if not Temp then
                  pragma Debug (O ("FATAL: Invoke_On_Servant failed"));
                  Result.Fatal := True;
               end if;
            end;

         exception
            when CORBA.Bad_Param =>
               Result.Default_Servant_No_Id := False;
               pragma Debug (O ("Default_Servant_No_Id failed"));
         end;

         --  Test invocation on default servant with User Id

         begin
            declare
               Obj_Ref2 : constant Echo.Ref :=
                 Echo.Helper.To_Ref
                 (Create_Reference_With_Id
                  (POA,
                   PortableServer.String_To_ObjectId ("dead"),
                   To_CORBA_String (Echo.Repository_Id)));
            begin
               Temp := Invoke_On_Servant (Obj_Ref2);

               if not Temp then
                  pragma Debug (O ("FATAL: Invoke_On_Servant failed"));
                  Result.Fatal := True;
               end if;
            end;

         exception
            when CORBA.Bad_Param =>
               Result.Default_Servant_Id := False;
               pragma Debug (O ("Default_Servant_Id failed"));
         end;

      exception
         when PortableServer.POA.WrongPolicy =>
            Result.Default_Servant_Id := False;
            pragma Debug (O ("Default_Servant_Id failed"));

            Result.Default_Servant_No_Id := False;
            pragma Debug (O ("Default_Servant_No_Id failed"));

         when E : others =>
            pragma Debug (O ("Got exception B"
                             & Exception_Name (E)
                             & " "
                             & Exception_Message (E)));
            Result.Fatal := True;
      end;

      --  End of tests

      return Result;

   exception
      when E : others =>
         pragma Debug (Put_Line ("Got exception @end "
                                 & Exception_Name (E)
                                 & " "
                                 & Exception_Message (E)));
         Result.Fatal := True;

         return Result;

   end Test_POA_Activation_Policies;

   --------------------
   -- Analyze_Result --
   --------------------

   function Analyze_Result
     (Result : Result_Vector;
      Tp     : ThreadPolicyValue;
      Lp     : LifespanPolicyValue;
      Up     : IdUniquenessPolicyValue;
      Ap     : IdAssignmentPolicyValue;
      Ip     : ImplicitActivationPolicyValue;
      Sp     : ServantRetentionPolicyValue;
      Rp     : RequestProcessingPolicyValue)
     return Boolean;

   function Analyze_Result
     (Result : Result_Vector;
      Tp     : ThreadPolicyValue;
      Lp     : LifespanPolicyValue;
      Up     : IdUniquenessPolicyValue;
      Ap     : IdAssignmentPolicyValue;
      Ip     : ImplicitActivationPolicyValue;
      Sp     : ServantRetentionPolicyValue;
      Rp     : RequestProcessingPolicyValue)
     return Boolean
   is
      pragma Unreferenced (Tp, Lp);
   begin
      if Result.Fatal then
         Output ("Result.Fatal", True);
         return False;
      end if;

      if Result.Implicit_Activation
        and then (Sp /= RETAIN
                  or Ip /= IMPLICIT_ACTIVATION)
      then
         Output ("Result.Implicit_Activation", True);
         return False;
      end if;

      if not Result.Get_Type_Id then
         Output ("Result.Get_Type_Id", True);
         return False;
      end if;

      if Result.Activation_No_Id
        and then Ap /= SYSTEM_ID
        and then Sp /= RETAIN
      then
         Output ("Resuilt.Activation_No_Id", True);
         return False;
      end if;

      if Result.Unique_Activation_No_Id
        and then Up /= UNIQUE_ID
      then
         Output ("Result.Unique_Activation_No_Id", True);
         return False;
      end if;

      if Result.Deactivation_No_Id
        and then not Result.Activation_No_Id
        and then Sp /= RETAIN
      then
         Output ("Result.Deactivation_No_Id", True);
         return False;
      end if;

      if Result.Activation_Id
        and then Sp /= RETAIN
      then
         Output ("Result.Activation_Id", True);
         return False;
      end if;

      if Result.Unique_Activation_Id
        and then Up /= UNIQUE_ID
      then
         Output ("Result.Unique_Activation_Id", True);
         return False;
      end if;

      if Result.Deactivation_Id
        and then not Result.Activation_Id
        and then Sp /= RETAIN
      then
         Output ("Result.Deactivation_Id", True);
         return False;
      end if;

      if Result.Default_Servant_No_Id
        and then Rp /= USE_DEFAULT_SERVANT
        and then Ap /= SYSTEM_ID
      then
         Output ("Result.Use_Default_Servant_No_Id", True);
         return False;
      end if;

      if Result.Default_Servant_Id
        and then Rp /= USE_DEFAULT_SERVANT
        and then Ap /= USER_ID
      then
         Output ("Result.Use_Default_Servant_Id", True);
         return False;
      end if;

      return True;
   end Analyze_Result;

   ------------------
   -- Test_POA_API --
   ------------------

   procedure Test_POA_API
   is
      Test_POA : PortableServer.POA.Local_Ref;

      Result : Boolean := True;
      Temp : Boolean;

   begin
      New_Test ("POA API");

      for Lp in LifespanPolicyValue'Range loop
         for Up in IdUniquenessPolicyValue'Range loop
            for Ap in IdAssignmentPolicyValue'Range loop
               for Ip in ImplicitActivationPolicyValue'Range loop
                  for Sp in ServantRetentionPolicyValue'Range loop
                     for Rp in USE_ACTIVE_OBJECT_MAP_ONLY ..
                       USE_DEFAULT_SERVANT loop

                        if Are_Policies_Valid
                          (ORB_CTRL_MODEL, Lp, Up, Ap, Ip, Sp, Rp) then

                           pragma Debug (O (" "));
                           pragma Debug (O ("Testing: " &
                                            Policies_Image
                                            (ORB_CTRL_MODEL,
                                             Lp, Up, Ap, Ip, Sp, Rp)));

                           Test_POA := Create_POA_With_Policies
                             (ORB_CTRL_MODEL, Lp, Up, Ap, Ip, Sp, Rp);

                           Temp := Analyze_Result
                             (Test_POA_Activation_Policies (Test_POA),
                              ORB_CTRL_MODEL, Lp, Up, Ap, Ip, Sp, Rp);

                           PortableServer.POA.Destroy
                             (Test_POA, False, False);

                           Result := Result and Temp;

                           if not Temp then
                              null;
                              pragma Debug (O ("Not Ok"));
                           end if;
                        end if;

                     end loop;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;

      Output ("Test_POA_API", Result);
   end Test_POA_API;

   ------------------------
   -- Test_POA_Hierarchy --
   ------------------------

   procedure Test_POA_Hierarchy
   is
      Policies : CORBA.Policy.PolicyList;

      Root_POA : constant PortableServer.POA.Local_Ref :=
        PortableServer.POA.Helper.To_Local_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      Child_POA : PortableServer.POA.Local_Ref;
      Huey_POA, Dewey_POA, Louie_POA : PortableServer.POA.Local_Ref;
      --  pragma Unreferenced (Huey_POA, Louie_POA);
      pragma Warnings (Off, Huey_POA);  --  WAG:5.02 DB08-008
      pragma Warnings (Off, Louie_POA); --  WAG:5.02 DB08-008
      --  Assigned but never read

   begin
      New_Test ("POA Hierarchy");

      --  Register Children POA

      Child_POA := PortableServer.POA.Local_Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          To_CORBA_String ("Child_POA"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies));

      Huey_POA := PortableServer.POA.Local_Ref
        (PortableServer.POA.Create_POA
         (Child_POA,
          To_CORBA_String ("Huey_POA"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies));

      Dewey_POA := PortableServer.POA.Local_Ref
        (PortableServer.POA.Create_POA
         (Child_POA,
          To_CORBA_String ("Dewey_POA"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies));

      Louie_POA := PortableServer.POA.Local_Ref
        (PortableServer.POA.Create_POA
         (Dewey_POA,
          To_CORBA_String ("Louie_POA"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies));

      Output ("Registred POA tree", True);

      --  Test Find_POA

      declare
         Huey_2 : PortableServer.POA.Local_Ref;
         --  pragma Unreferenced (Huey_2);
         pragma Warnings (Off, Huey_2); --  WAG:5.02 DB08-008
         --  Assigned but never read
      begin
         Huey_2 := PortableServer.POA.Local_Ref
           (PortableServer.POA.Find_POA
            (Child_POA,
             To_CORBA_String ("Huey_POA"),
             False));
         Output ("Find_POA on an existent POA", True);
      end;

      declare
         Donald : PortableServer.POA.Local_Ref;
         --  pragma Unreferenced (Donald);
         pragma Warnings (Off, Donald); --  WAG:5.02 DB08-008
         --  Assigned but never read

      begin
         Donald := PortableServer.POA.Local_Ref
           (PortableServer.POA.Find_POA
            (Child_POA,
             To_CORBA_String ("Donald_POA"),
             True));
      exception
         when PortableServer.POA.AdapterNonExistent =>
            Output ("Find_POA on a non existent POA", True);
      end;

      --  Test Get_The_Children

      declare
         use PortableServer.IDL_SEQUENCE_PortableServer_POA_Forward;

         Children : constant PortableServer.POAList
           := PortableServer.POA.Get_The_Children (Child_POA);

         Children_Array : constant Element_Array
           := To_Element_Array (Children);

      begin
         if Children_Array'Length /= 2 then
            raise Program_Error;
         end if;

         for J in Children_Array'Range loop
            Output ("Found child: "
                    & CORBA.To_Standard_String
                    (PortableServer.POA.Get_The_Name
                     (PortableServer.POA.Convert.To_Ref (Children_Array (J)))),
                     True);
         end loop;

      end;

      Destroy (Child_POA, True, True);
      Output ("Destroy POA tree", True);

   end Test_POA_Hierarchy;

   --------------
   -- Test_OID --
   --------------

   procedure Test_OID is
      use CORBA.Policy;
      use CORBA.Policy.IDL_SEQUENCE_Policy;

      Root_POA  : constant PortableServer.POA.Local_Ref
        := PortableServer.POA.Helper.To_Local_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      My_POA, My_Child_POA : PortableServer.POA.Local_Ref;
      My_POA_Manager, My_Child_POA_Manager :
        PortableServer.POAManager.Local_Ref;

      Success : Boolean;
   begin
      New_Test ("OID");

      declare
         Policies : CORBA.Policy.PolicyList;

         Lifespan_Policy : constant CORBA.Policy.Ref
           := CORBA.Policy.Ref
           (Create_Lifespan_Policy (PERSISTENT));

         Id_Assignment_Policy : constant CORBA.Policy.Ref
           := CORBA.Policy.Ref
           (Create_Id_Assignment_Policy (USER_ID));

         Implicit_Activation_Policy : constant CORBA.Policy.Ref
           := CORBA.Policy.Ref
           (Create_Implicit_Activation_Policy (NO_IMPLICIT_ACTIVATION));

         Request_Processing_Policy : constant CORBA.Policy.Ref
           := CORBA.Policy.Ref
           (Create_Request_Processing_Policy (USE_SERVANT_MANAGER));

      begin
         Append (Policies, Lifespan_Policy);
         Append (Policies, Id_Assignment_Policy);
         Append (Policies, Implicit_Activation_Policy);
         Append (Policies, Request_Processing_Policy);
         My_POA :=
           PortableServer.POA.Local_Ref
           (PortableServer.POA.Create_POA
            (Root_POA,
             CORBA.To_CORBA_String ("Child"),
             My_POA_Manager,
             Policies));
         PortableServer.POAManager.Activate
           (PortableServer.POA.Get_The_POAManager (My_POA));

         Output ("Created POA Child_POA", True);

         My_Child_POA :=
           PortableServer.POA.Local_Ref
           (PortableServer.POA.Create_POA
            (My_POA,
             CORBA.To_CORBA_String ("My_POA"),
             My_Child_POA_Manager,
             Policies));
         PortableServer.POAManager.Activate
           (PortableServer.POA.Get_The_POAManager (My_Child_POA));

         Output ("Created POA Child_POA/My_POA", True);

      end;

      declare
         Srv  : constant Echo.Impl.Object_Acc := new Echo.Impl.Object;
         Srv2 : constant Echo.Impl.Object_Acc := new Echo.Impl.Object;
         Id   : constant PortableServer.ObjectId
           := PortableServer.String_To_ObjectId ("123");
         Ref  : CORBA.Object.Ref;
      begin
         PortableServer.POA.Activate_Object_With_Id
           (My_Child_POA, Id, PortableServer.Servant (Srv));
         Output ("Activate_Object_With_Id", True);

         Ref := Servant_To_Reference
           (My_Child_POA, PortableServer.Servant (Srv));
         Output ("Servant_To_Reference", True);

         begin
            declare
               Id : constant PortableServer.ObjectId
                 := Reference_To_Id (Root_POA, Ref);
               pragma Unreferenced (Id);

            begin
               Output ("Reference_To_Id raised "
                       & "PortableServer.POA.WrongAdapter",
                       False);
            end;
         exception
            when PortableServer.POA.WrongAdapter =>
               Output ("Reference_To_Id raised "
                       & "PortableServer.POA.WrongAdapter",
                       True);
            when E : others =>
               Output ("Reference_To_Id raised wrong exception: "
                       & Ada.Exceptions.Exception_Name (E), False);
         end;

         begin
            PortableServer.POA.Activate_Object_With_Id
              (My_Child_POA, Id, PortableServer.Servant (Srv));
            Output ("Activate_Object_With_Id: same Id, Servant "
                    & "raised ServantAlreadyActive", False);
         exception
            when PortableServer.POA.ServantAlreadyActive =>
               Output ("Activate_Object_With_Id: same Id, Servant "
                       & "raised ServantAlreadyActive", True);
         end;

         begin
            PortableServer.POA.Activate_Object_With_Id
              (My_Child_POA, Id, PortableServer.Servant (Srv2));
            Output ("Activate_Object_With_Id with the same Id "
                    & "raised no exception", False);
         exception
            when PortableServer.POA.ObjectAlreadyActive =>
               Output ("Activate_Object_With_Id with the same Id "
                       & "raised ObjectAlreadyActive", True);
         end;

         Ref := Servant_To_Reference
           (My_Child_POA, PortableServer.Servant (Srv));
         Output ("Servant_To_Reference", True);

         begin
            declare
               Id : constant PortableServer.ObjectId
                 := Reference_To_Id (Root_POA, Ref);
               pragma Unreferenced (Id);

            begin
               Output ("Reference_To_Id raised "
                       & "PortableServer.POA.WrongAdapter",
                       False);
            end;
         exception
            when PortableServer.POA.WrongAdapter =>
               Output ("Reference_To_Id raised "
                       & "PortableServer.POA.WrongAdapter",
                       True);
            when E : others =>
               Output ("Reference_To_Id raised wrong exception: "
                       & Ada.Exceptions.Exception_Name (E), False);
         end;

         declare
            Oid : constant PortableServer.ObjectId
              := Reference_To_Id (My_Child_POA, Ref);
         begin
            Output ("Reference_To_Id raised no exception", True);
            Ada.Text_IO.Put_Line
              ("OID: "
               & PortableServer.ObjectId_To_String (Oid));
            Output ("OID is correct", Id = Oid);
         exception
            when others =>
               Output ("Reference_To_Id raised an exception", False);
         end;

      end;

      --  Set up My_POA with MULTIPLE_ID policy

      declare
         Id_Uniqueness : constant CORBA.Policy.Ref
           := CORBA.Policy.Ref
               (PortableServer.POA.Create_Id_Uniqueness_Policy
                 (PortableServer.MULTIPLE_ID));
         Id_Assignment : constant CORBA.Policy.Ref
           := CORBA.Policy.Ref
               (PortableServer.POA.Create_Id_Assignment_Policy
                 (PortableServer.USER_ID));
         Activation    : constant CORBA.Policy.Ref
           := CORBA.Policy.Ref
               (PortableServer.POA.Create_Implicit_Activation_Policy
                 (PortableServer.NO_IMPLICIT_ACTIVATION));
         Processing    : constant CORBA.Policy.Ref
           := CORBA.Policy.Ref
               (PortableServer.POA.Create_Request_Processing_Policy
                 (PortableServer.USE_DEFAULT_SERVANT));
         Policies      : CORBA.Policy.PolicyList;

      begin
         PortableServer.POAManager.Activate
           (PortableServer.POA.Get_The_POAManager (Root_POA));

         CORBA.Policy.IDL_SEQUENCE_Policy.Append (Policies, Id_Uniqueness);
         CORBA.Policy.IDL_SEQUENCE_Policy.Append (Policies, Id_Assignment);
         CORBA.Policy.IDL_SEQUENCE_Policy.Append (Policies, Activation);
         CORBA.Policy.IDL_SEQUENCE_Policy.Append (Policies, Processing);

         My_POA :=
           PortableServer.POA.Local_Ref
           (PortableServer.POA.Create_POA
            (Root_POA,
             CORBA.To_CORBA_String ("My_POA_2"),
             PortableServer.POA.Get_The_POAManager (Root_POA),
             Policies));
         PortableServer.POA.Set_Servant (My_POA, new Echo.Impl.Object);
      end;

      declare
         Oid_1  : PortableServer.ObjectId;
         My_Ref : CORBA.Object.Ref;

      begin
         PortableServer.Append (Oid_1, CORBA.Octet'(1));

         My_Ref :=
           PortableServer.POA.Create_Reference_With_Id
           (My_POA, Oid_1, CORBA.To_CORBA_String (Echo.Repository_Id));

         Oid_1 := PortableServer.POA.Reference_To_Id (My_POA, My_Ref);
         Success := True;

      exception
         when E : others =>
            Put_Line ("POA::reference_to_id test raised "
                        & Ada.Exceptions.Exception_Information (E));
            Success := False;
      end;

      PolyORB.Utils.Report.Output ("Reference_To_Id (multiple id)", Success);
   end Test_OID;

end Test000_Setup;
