-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.adacore.com/polyorb/)
-------------------------------------------------
pragma Style_Checks (Off);

with CORBA;
with PortableServer;

package i1.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   function get_val1
     (Self : access Object)
     return CORBA.Float;

   procedure set_val1
     (Self : access Object;
      To : in CORBA.Float);

   function get_val2
     (Self : access Object)
     return CORBA.Float;

   procedure set_val2
     (Self : access Object;
      To : in CORBA.Float);

   function get_tab_val
     (Self : access Object)
     return CORBA.Float;

   procedure set_tab_val
     (Self : access Object;
      To : in CORBA.Float);

   function get_tab
     (Self : access Object)
     return i1.Tab_Float;

   procedure set_tab
     (Self : access Object;
      To : in i1.Tab_Float);

private

   type Object is
     new PortableServer.Servant_Base with record
      --  Insert components to hold the state
      --  of the implementation object.
      null;
   end record;

end i1.Impl;
