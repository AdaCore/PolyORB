with CORBA;
with PortableServer;
with Broca.Buffers;
package myexceptions.Skel is

   type Object is abstract new PortableServer.Servant_Base
     with null record;

private
   function Get_Type_Id (Obj : Object) return CORBA.RepositoryId;
   procedure Giop_Dispatch
     (Obj : access Object;
      Operation : String;
      Request_Id : CORBA.Unsigned_Long;
      Response_Expected : CORBA.Boolean;
      Stream : in out Broca.Buffers.Buffer_Descriptor);
end myexceptions.Skel;
