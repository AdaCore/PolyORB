with Broca.Environment;            use Broca.Environment;
with Broca.ORB;                    use Broca.ORB;
with Broca.Inet_Server;
with Broca.Server_Tools;
with CORBA.Object;
with CosNaming.NamingContext.Impl; use CosNaming.NamingContext.Impl;
with PortableServer;

package body AB_Names is

   type NamingContext_Ptr is access CosNaming.NamingContext.Impl.Object;

   Root_NC  : constant NamingContext_Ptr := new Object;

   Ref      : CORBA.Object.Ref;

   Port_Str : constant String := Get_Conf (Naming_Port, Naming_Port_Default);

begin
   Broca.Inet_Server.Start (Natural'Value (Port_Str));
   Broca.Server_Tools.Initiate_Server;
   Broca.Server_Tools.Initiate_Servant
     (PortableServer.Servant (Root_NC), Ref);
   Register_Initial_Reference (Name_Service_ObjectId, Ref);
end AB_Names;
