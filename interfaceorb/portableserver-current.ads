
package PortableServer.Current is

   type Ref is new CORBA.Current.Ref with null record;

   NoContext : exception;

   type NoContext_Members is
    new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members (From : in CORBA.Exception_Occurrence;
                          To   : out NoContext_Members);

   function Get_POA (Self : Ref)
     return PortableServer.POA_Forward.Ref;

   function Get_Object_Id (Self : Ref) return ObjectId;

end PortableServer.Current;
