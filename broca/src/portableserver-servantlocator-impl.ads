with PortableServer.ServantManager.Impl;

package PortableServer.ServantLocator.Impl is

   type Object is abstract new PortableServer.ServantManager.Impl.Object with
     private;

   procedure Preinvoke
     (Self : in out Object;
      Oid : in ObjectId;
      Adapter : in PortableServer.POA.Ref;
      Operation : in CORBA.Identifier;
      The_Cookie : out Cookie;
      Returns : out Servant) is abstract;

   procedure Postinvoke
     (Self : in out Object;
      Oid : in ObjectId;
      Adapter : in PortableServer.POA.Ref;
      Operation : in CORBA.Identifier;
      The_Cookie : in Cookie;
      The_Servant : in Servant) is abstract;

private
   type Object is abstract new PortableServer.ServantManager.Impl.Object with
     null record;
end PortableServer.ServantLocator.Impl;
