--  Helper functions for CORBA servers.

--  $Id$

with Droopi.Setup.Test; use Droopi.Setup.Test;
with Droopi.Setup.Test_CORBA;
with Droopi.No_Tasking;
with Droopi.ORB.Task_Policies;

with CORBA.Impl;
pragma Warnings (Off, CORBA.Impl);
with CORBA.Object;
pragma Warnings (Off, CORBA.Object);
with CORBA.AbstractBase;
pragma Warnings (Off, CORBA.AbstractBase);

with CORBA;
with CORBA.ORB;

with PortableServer.POA;
with PortableServer.POAManager;
pragma Elaborate_All (PortableServer.POA);

with Droopi.Log;
pragma Elaborate_All (Droopi.Log);

package body Droopi.CORBA_P.Server_Tools is

   use Droopi.Log;

   package L is new Droopi.Log.Facility_Log ("droopi.corba_p.server_tools");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   Root_POA : PortableServer.POA.Ref;

--    task type ORBTask is
--       pragma Storage_Size
--         (Droopi.CORBA_P.Parameters.Server_Tasks_Storage_Size);
--    end ORBTask;
--    type ORBTaskPtr is access ORBTask;

--    task body ORBTask is
--    begin
--       CORBA.ORB.Run;
--    end ORBTask;

   procedure Initiate_RootPOA;

   -----------
   -- Debug --
   -----------


   Setup_Done : Boolean := False;

   procedure Ensure_Setup;

   procedure Ensure_Setup is
   begin
      if Setup_Done then
         return;
      end if;
      Setup_Done := True;

      Initialize_Test_Server
        (Droopi.No_Tasking.Initialize'Access,
         new Droopi.ORB.Task_Policies.No_Tasking);
      Initialize_Test_Access_Points;
      Setup.Test_CORBA.Initialize_CORBA_Test_Object;
   end Ensure_Setup;

   ----------------------
   -- Initiate_RootPOA --
   ----------------------

   procedure Initiate_RootPOA is
      RootPOAStr  : CORBA.String;
   begin
      Ensure_Setup;

      RootPOAStr := CORBA.To_CORBA_String ("RootPOA");
      Root_POA   := PortableServer.POA.To_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.ObjectId (RootPOAStr)));
   end Initiate_RootPOA;

   ---------------------
   -- Initiate_Server --
   ---------------------

   procedure Initiate_Server (Start_New_Task : Boolean := True)
   is
      --  ORBMainLoop : ORBTaskPtr;
   begin
      Ensure_Setup;

      if CORBA.Object.Is_Nil (CORBA.Object.Ref (Root_POA)) then
         Initiate_RootPOA;
      end if;

      PortableServer.POAManager.Activate
        (PortableServer.POA.Get_The_POAManager (Root_POA));

      --  if Start_New_Task then
      --     ORBMainLoop := new ORBTask;
      --  else
      CORBA.ORB.Run;
      --  end if;
   end Initiate_Server;

   ----------------------
   -- Initiate_Servant --
   ----------------------

   procedure Initiate_Servant
     (S : in PortableServer.Servant;
      R : out CORBA.Object.Ref'Class) is
   begin
      pragma Debug (O ("Initiate_Servant : enter"));
      Ensure_Setup;

      if CORBA.Object.Is_Nil (CORBA.Object.Ref (Root_POA)) then
         Initiate_RootPOA;
      end if;
      pragma Debug (O ("Initiate_Servant : ready to "
                       & "call CORBA.Object.Set"));
      CORBA.Object.Set
        (CORBA.Object.Ref (R),
         CORBA.Object.Object_Of
         (PortableServer.POA.Servant_To_Reference (Root_POA, S)));
      pragma Debug (O ("Initiate_Servant : end"));
   end Initiate_Servant;

   --------------------------
   -- Reference_To_Servant --
   --------------------------

   procedure Reference_To_Servant
     (R : in CORBA.Object.Ref'Class;
      S : out PortableServer.Servant) is
   begin
      if CORBA.Object.Is_Nil (CORBA.Object.Ref (Root_POA)) then
         Initiate_RootPOA;
      end if;

      S := PortableServer.POA.Reference_To_Servant
        (Root_POA, CORBA.Object.Ref (R));
   end Reference_To_Servant;

   --------------------------
   -- Servant_To_Reference --
   --------------------------

   procedure Servant_To_Reference
     (S : in PortableServer.Servant;
      R : out CORBA.Object.Ref'Class) renames Initiate_Servant;

end Droopi.CORBA_P.Server_Tools;
