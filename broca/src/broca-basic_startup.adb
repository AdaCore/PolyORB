with CORBA;
with CORBA.ORB;

with PortableServer.POA;
with PortableServer.POAManager;

with Broca.RootPOA;
pragma Elaborate (Broca.RootPOA);
pragma Elaborate_All (PortableServer.POA);

package body Broca.Basic_Startup is

   Root_POA : PortableServer.POA.Ref;

   task type ORBTask;
   type ORBTaskPtr is access ORBTask;

   task body ORBTask is
   begin
      CORBA.ORB.Run;
   end ORBTask;

   procedure Initiate_RootPOA;

   ----------------------
   -- Initiate_RootPOA --
   ----------------------

   procedure Initiate_RootPOA is
      RootPOAStr  : CORBA.String;

   begin
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
      if CORBA.Object.Is_Null (CORBA.Object.Ref (Root_POA)) then
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
      R : out CORBA.Object.Ref) is
   begin
      if CORBA.Object.Is_Null (CORBA.Object.Ref (Root_POA)) then
         Initiate_RootPOA;
      end if;

      R := PortableServer.POA.Servant_To_Reference (Root_POA, S);
   end Initiate_Servant;

end Broca.Basic_Startup;
