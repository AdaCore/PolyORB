-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
-------------------------------------------------
pragma Style_Checks (Off);

with CORBA;
with PortableServer;

package module.m11.I111.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   function get_value
     (Self : access Object)
     return CORBA.Float;

   procedure set_value
     (Self : access Object;
      To : in CORBA.Float);

private

   type Object is
     new PortableServer.Servant_Base with record
      --  Insert components to hold the state
      --  of the implementation object.
      null;
   end record;

end module.m11.I111.Impl;
