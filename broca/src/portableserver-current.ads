with CORBA;
with CORBA.Current;
with PortableServer; use PortableServer;

package PortableServer.Current is
   type Ref is new CORBA.Current.Ref with private;

   NoContext : exception;
   type NoContext_Members is new CORBA.IDL_Exception_Members
     with null record;
   procedure Get_Members
     (From : in  CORBA.Exception_Occurrence;
      To   : out NoContext_Members);

   function get_POA (Self : Ref)
     return PortableServer.POA_Forward.Ref;

   function get_object_id (Self : Ref)
     return ObjectId;

private
   type Ref is new CORBA.Current.Ref with null record;
end PortableServer.Current;
