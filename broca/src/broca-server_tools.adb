with CORBA;
with CORBA.ORB;

with PortableServer.POA;
with PortableServer.POAManager;

with Broca.Inet_Server;

with Broca.Parameters;

with Broca.RootPOA;
pragma Elaborate (Broca.RootPOA);
pragma Elaborate_All (PortableServer.POA);

with Broca.Debug;

package body Broca.Server_Tools is

   Root_POA : PortableServer.POA.Ref;

   task type ORBTask is
      pragma Storage_Size (Broca.Parameters.Server_Tasks_Storage_Size);
   end ORBTask;
   type ORBTaskPtr is access ORBTask;

   task body ORBTask is
   begin
      CORBA.ORB.Run;
   end ORBTask;

   procedure Initiate_RootPOA;

   -----------
   -- Debug --
   -----------

   Flag : constant Natural
     := Broca.Debug.Is_Active ("broca.server_tools");
   procedure O is new Broca.Debug.Output (Flag);

   ----------------------
   -- Initiate_RootPOA --
   ----------------------

   procedure Initiate_RootPOA is
      RootPOAStr  : CORBA.String;
   begin
      Broca.Inet_Server.Ensure_Started;

      RootPOAStr := CORBA.To_CORBA_String ("RootPOA");
      Root_POA   := PortableServer.POA.To_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.ObjectId (RootPOAStr)));
   end Initiate_RootPOA;

   ---------------------
   -- Initiate_Server --
   ---------------------

   procedure Initiate_Server is
      ORBMainLoop : ORBTaskPtr;

   begin
      Broca.Inet_Server.Ensure_Started;

      if CORBA.Object.Is_Nil (CORBA.Object.Ref (Root_POA)) then
         Initiate_RootPOA;
      end if;

      PortableServer.POAManager.Activate
        (PortableServer.POA.Get_The_POAManager (Root_POA));
      ORBMainLoop := new ORBTask;
   end Initiate_Server;

   ----------------------
   -- Initiate_Servant --
   ----------------------

   procedure Initiate_Servant
     (S : in PortableServer.Servant;
      R : out CORBA.Object.Ref'Class) is
   begin
      pragma Debug (O ("Initiate_Servant : enter"));
      Broca.Inet_Server.Ensure_Started;

      if CORBA.Object.Is_Nil (CORBA.Object.Ref (Root_POA)) then
         Initiate_RootPOA;
      end if;
      pragma Debug (O ("Initiate_Servant : ready to "
                       & "call Corba.Object.Set"));
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

end Broca.Server_Tools;
