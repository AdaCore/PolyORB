
package PortableServer.ServantLocator is

   type Ref is new PortableServer.ServantManager.Ref
     with null record;

   type Cookie_Base is new Implementation_Defined;
   -- ... implementation defined, tagged type Cookie is access all
   --  Cookie_Base'CLASS;

   procedure Preinvoke
     (Self : in Ref;
      Oid : in ObjectId;
      Adapter : in PortableServer.POA.Ref;
      Operation : in CORBA.Identifier;
      The_Cookie : out Cookie;
      Returns : out Servant);

   procedure Postinvoke
     (Self : in Ref;
      Oid : in ObjectId;
      Adapter : in PortableServer.POA.Ref;
      Operation : in CORBA.Identifier;
      The_Cookie : in Cookie;
      The_Servant : in Servant);

end PortableServer.ServantLocator;
