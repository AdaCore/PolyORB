with CORBA;
with PortableServer;
with Broca.Types;
package Echo.Impl is

   type Object is abstract new PortableServer.Servant_Base
     with null record;

   function echoString
     (Self : access Object;
      Mesg : in CORBA.String)
      return CORBA.String is abstract;

private
   function Get_Type_Id (Obj : Object) return CORBA.RepositoryId;
   procedure Giop_Dispatch
     (Obj : access Object;
      Operation : String;
      Request_Id : CORBA.Unsigned_Long;
      Reponse_Expected : CORBA.Boolean;
      Stream : in out Broca.Types.Buffer_Descriptor);
end Echo.Impl;
