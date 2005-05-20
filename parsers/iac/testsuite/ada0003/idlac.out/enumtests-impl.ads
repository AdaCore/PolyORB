-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.adacore.com/polyorb/)
-------------------------------------------------
pragma Style_Checks (Off);

with PortableServer;

package EnumTests.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   function get_attr_enum
     (Self : access Object)
     return EnumTests.Color;

   procedure set_attr_enum
     (Self : access Object;
      To : in EnumTests.Color);

   procedure modif_enum
     (Self : access Object;
      C : in out EnumTests.Color;
      Returns : out EnumTests.Color);

private

   type Object is
     new PortableServer.Servant_Base with record
      --  Insert components to hold the state
      --  of the implementation object.
      null;
   end record;

end EnumTests.Impl;
