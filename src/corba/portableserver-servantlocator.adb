package body PortableServer.ServantLocator is

   ---------------
   -- Preinvoke --
   ---------------

   procedure Preinvoke
     (Self       : in  Ref;
      Oid        : in  ObjectId;
      Adapter    : in  PortableServer.POA_Forward.Ref;
      Operation  : in  CORBA.Identifier;
      The_Cookie : out Cookie;
      Returns    : out Servant)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Self);
      pragma Unreferenced (Oid);
      pragma Unreferenced (Adapter);
      pragma Unreferenced (Operation);
      pragma Warnings (On); --  WAG:3.15

      Result_Cookie : constant Cookie_Base
        := Cookie_Base'(Root_Cookie with null record);

   begin
      The_Cookie := new Cookie_Base'(Result_Cookie);
      Returns := null;

   end Preinvoke;

   ----------------
   -- Postinvoke --
   ----------------

   procedure Postinvoke
     (Self        : in Ref;
      Oid         : in ObjectId;
      Adapter     : in PortableServer.POA_Forward.Ref;
      Operation   : in CORBA.Identifier;
      The_Cookie  : in Cookie;
      The_Servant : in Servant)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Self);
      pragma Unreferenced (Oid);
      pragma Unreferenced (Adapter);
      pragma Unreferenced (Operation);
      pragma Unreferenced (The_Cookie);
      pragma Unreferenced (The_Servant);
      pragma Warnings (On); --  WAG:3.15

   begin
      null;
   end Postinvoke;

end PortableServer.ServantLocator;
