-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
-------------------------------------------------
pragma Style_Checks (Off);

with CORBA;
with PortableServer;

package m2.I21.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   function is_greater
     (Self : access Object;
      f1 : in CORBA.Float;
      f2 : in CORBA.Float)
     return m2.I21.new_bool;

private

   type Object is
     new PortableServer.Servant_Base with record
      --  Insert components to hold the state
      --  of the implementation object.
      null;
   end record;

end m2.I21.Impl;
