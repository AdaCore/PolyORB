with CORBA.Object;
with PortableServer;

package Broca.Basic_Startup is

   pragma Elaborate_Body;

   procedure Initiate_Server;

   procedure Initiate_Servant
     (S : in PortableServer.Servant;
      R : out CORBA.Object.Ref);

end Broca.Basic_Startup;
