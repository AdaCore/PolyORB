--  Helper functions for CORBA servers.

--  $Id$

with Droopi.Obj_Adapters;
with Droopi.POA;
with Droopi.POA.Basic_POA; use Droopi.POA.Basic_POA;
with Droopi.POA_Config;
with Droopi.POA_Config.Minimum;
--  XXX hardcoded configuration!!!!!!
with Droopi.Setup;
with Droopi.Setup.Test; use Droopi.Setup.Test;
with Droopi.Smart_Pointers;
with Droopi.No_Tasking;
--  XXX hardcoded tasking policy!!!
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
   end Ensure_Setup;

   ----------------------
   -- Initiate_RootPOA --
   ----------------------

   procedure Initiate_RootPOA is
      --  RootPOAStr  : CORBA.String;
      Obj_Adapter : Droopi.POA.Obj_Adapter_Access;
   begin
      Ensure_Setup;

      pragma Debug (O ("Initializing OA configuration... "));
      Droopi.POA_Config.Set_Configuration
        (new Droopi.POA_Config.Minimum.Minimum_Configuration);
      pragma Debug (O ("Creating object adapter... "));
      Obj_Adapter := new Droopi.POA.Basic_POA.Basic_Obj_Adapter;
      Droopi.POA.Basic_POA.Create (Basic_Obj_Adapter (Obj_Adapter.all)'Access);
      --  Create object adapter

      Droopi.ORB.Set_Object_Adapter
        (Droopi.Setup.The_ORB,
         Droopi.Obj_Adapters.Obj_Adapter_Access (Obj_Adapter));
      --  Link object adapter with ORB.

      PortableServer.POA.Set
        (Root_POA, Droopi.Smart_Pointers.Entity_Ptr (Obj_Adapter));

--       RootPOAStr := CORBA.To_CORBA_String ("RootPOA");
--       Root_POA   := PortableServer.POA.To_Ref
--         (CORBA.ORB.Resolve_Initial_References
--          (CORBA.ORB.ObjectId (RootPOAStr)));
      --  XXX Should REGISTER initial ref for the root POA.
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
