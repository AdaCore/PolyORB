-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.adacore.com/polyorb/)
-------------------------------------------------
pragma Style_Checks (Off);

with m1.i1;
with PortableServer;

package m1.i2.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   function get_attr1
     (Self : access Object)
     return m1.i1.t1;

   procedure set_attr1
     (Self : access Object;
      To : in m1.i1.t1);

   function get_bool
     (Self : access Object)
     return m1.b1;

   procedure set_bool
     (Self : access Object;
      To : in m1.b1);

private

   type Object is
     new PortableServer.Servant_Base with record
      --  Insert components to hold the state
      --  of the implementation object.
      null;
   end record;

end m1.i2.Impl;
