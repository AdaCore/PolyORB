with CORBA;
with PortableServer;
with Broca.Buffers;
package Echo.Skel is

   type Object is abstract new PortableServer.Servant_Base
     with null record;

   function echoString
     (Self : access Object;
      Mesg : in CORBA.String)
      return CORBA.String is abstract;

private
   function Get_Type_Id (Obj : Object) return CORBA.RepositoryId;
   procedure GIOP_Dispatch
     (Obj : access Object;
      Operation : String;
      Request_Id : CORBA.Unsigned_Long;
      Response_Expected : CORBA.Boolean;
      Request_Buffer : access Broca.Buffers.Buffer_Type;
      Reply_Buffer   : access Broca.Buffers.Buffer_Type);

end Echo.Skel;
