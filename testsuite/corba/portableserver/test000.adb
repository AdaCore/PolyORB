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

with CORBA.Object;
with CORBA.ORB;
with CORBA.Impl;

with PortableServer.POA;
with PortableServer.POAManager;

with Echo.Impl;

with Test_Job;

procedure Test000 is

   use Ada.Exceptions;
   use Ada.Text_IO;

   use CORBA;
   use PortableServer;
   use PortableServer.POA;
   use PortableServer.POAManager;

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
   --  Test POA Single_Thread Thread Policy;

   procedure Test_Main_Thread_Policy;
   --  Test POA Main_Thread Thread Policy;

   ---------------
   -- Test_Init --
   ---------------

   procedure Test_Init is
   begin
      --  ORB Initialization.
      CORBA.ORB.Initialize ("ORB");

      Initiate_Server (True);
      --  Run the ORB instance in a separated task.

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
      CORBA.Object.Set
        (CORBA.Object.Ref (Obj_Ref),
         CORBA.Object.Object_Of
         (Servant_To_Reference (To_POA,
                                PortableServer.Servant (Obj))));
      --  Not clear it is the correct way to do things ...

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
               IOR = Echo.EchoString_Reentrant (Obj_Ref, IOR));
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
      --  Get a reference on the RootPOA.
      --  XXX should use Get_Initial_References !
      Root_POA := Get_Root_POA;
      PolyORB.Report.Output ("Get Root_POA reference", True);

      Attach_Servant (Root_POA, Obj_Ref);
      Invoke_On_Servant (Obj_Ref);

   end Test_Root_POA;

   ---------------------
   -- Test_POAManager --
   ---------------------

   procedure Test_POAManager
   is
      Root_POA : constant PortableServer.POA.Ref := Get_Root_POA;

      Child_POA : PortableServer.POA.Ref;
      Obj_Ref  : Echo.Ref;
   begin
      PolyORB.Report.Output ("Begin tests on POAManager", True);

      --  Register a Child POA.

      Child_POA := PortableServer.POA.Ref (PortableServer.POA.Create_POA
        (Root_POA,
         To_CORBA_String ("Child_POA"),
         PortableServer.POA.Get_The_POAManager (Root_POA),
         PortableServer.ORB_CTRL_MODEL,
         PortableServer.TRANSIENT,
         PortableServer.UNIQUE_ID,
         PortableServer.SYSTEM_ID,
         PortableServer.NO_IMPLICIT_ACTIVATION,
         PortableServer.RETAIN,
         PortableServer.USE_ACTIVE_OBJECT_MAP_ONLY));
      PolyORB.Report.Output ("Created child POA", True);

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
      Root_POA : constant PortableServer.POA.Ref := Get_Root_POA;

      Child_POA : PortableServer.POA.Ref;
      Obj_Ref, Obj_Ref2  : Echo.Ref;
   begin
      PolyORB.Report.Output ("Begin tests of Single Thread Policy", True);

      --  Register a Child POA.

      Child_POA := PortableServer.POA.Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          To_CORBA_String ("Child_POA"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          PortableServer.SINGLE_THREAD_MODEL,
          PortableServer.TRANSIENT,
          PortableServer.UNIQUE_ID,
          PortableServer.SYSTEM_ID,
          PortableServer.NO_IMPLICIT_ACTIVATION,
          PortableServer.RETAIN,
          PortableServer.USE_ACTIVE_OBJECT_MAP_ONLY));

      PolyORB.Report.Output ("Created child POA with Single Thread policy",
                             True);

      --  Test the call is reentrant.

      --  XXX note that this test may work, but may also not, see
      --  PolyORB.POA_Policies.Thread_Policy.Single_Thread for more details.

      Attach_Servant (Child_POA, Obj_Ref);
      Invoke_On_Servant (Obj_Ref, True);

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
      Root_POA : constant PortableServer.POA.Ref := Get_Root_POA;

      Child_POA : PortableServer.POA.Ref;
      Obj_Ref, Obj_Ref2  : Echo.Ref;
   begin
      PolyORB.Report.Output ("Begin tests of Main Thread Policy", True);

      --  Register a Child POA.

      Child_POA := PortableServer.POA.Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          To_CORBA_String ("Child_POA"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          PortableServer.MAIN_THREAD_MODEL,
          PortableServer.TRANSIENT,
          PortableServer.UNIQUE_ID,
          PortableServer.SYSTEM_ID,
          PortableServer.NO_IMPLICIT_ACTIVATION,
          PortableServer.RETAIN,
          PortableServer.USE_ACTIVE_OBJECT_MAP_ONLY));

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

begin
   Test_Init;
   Test_Root_POA;
   Test_POAManager;
   Test_Single_Thread_Policy;
   Test_Main_Thread_Policy;

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

end Test000;
