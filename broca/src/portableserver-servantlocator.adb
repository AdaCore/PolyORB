with PortableServer.ServantManager;
with PortableServer.POA;
with PortableServer.ServantLocator.Impl;
with Broca.POA;

package body PortableServer.ServantLocator is

   procedure Preinvoke
     (Self : in Ref;
      Oid : in ObjectId;
      Adapter : in PortableServer.POA.Ref;
      Operation : in CORBA.Identifier;
      The_Cookie : out Cookie;
      Returns : out Servant) is
   begin
      PortableServer.ServantLocator.Impl.Preinvoke
        (Impl.Object'Class
         (Broca.POA.To_Internal_Skeleton (Self).P_Servant.all),
         Oid, Adapter, Operation, The_Cookie, Returns);
   end Preinvoke;

   procedure Postinvoke
     (Self : in Ref;
      Oid : in ObjectId;
      Adapter : in PortableServer.POA.Ref;
      Operation : in CORBA.Identifier;
      The_Cookie : in Cookie;
      The_Servant : in Servant) is
   begin
      PortableServer.ServantLocator.Impl.Postinvoke
        (Impl.Object'Class
         (Broca.POA.To_Internal_Skeleton (Self).P_Servant.all),
         Oid, Adapter, Operation, The_Cookie, The_Servant);
   end Postinvoke;
end PortableServer.ServantLocator;
