with CORBA.Object;
with PortableServer;

package Broca.Server_Tools is

   pragma Elaborate_Body;

   procedure Initiate_Server;

   procedure Initiate_Servant
     (S : in PortableServer.Servant;
      R : out CORBA.Object.Ref'Class);

   procedure Reference_To_Servant
     (R : in CORBA.Object.Ref'Class;
      S : out PortableServer.Servant);

   procedure Servant_To_Reference
     (S : in PortableServer.Servant;
      R : out CORBA.Object.Ref'Class);

end Broca.Server_Tools;
