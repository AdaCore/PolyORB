-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.adacore.com/polyorb/)
-------------------------------------------------
pragma Style_Checks (Off);

with PortableServer;

package mod1.Int1.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   function get_Real_Number
     (Self : access Object)
     return mod1.Int1.New_Float;

   procedure set_Real_Number
     (Self : access Object;
      To : in mod1.Int1.New_Float);

   function get_couleur
     (Self : access Object)
     return mod1.Int1.Color;

   procedure set_couleur
     (Self : access Object;
      To : in mod1.Int1.Color);

   function get_b1
     (Self : access Object)
     return mod1.bool;

   procedure set_b1
     (Self : access Object;
      To : in mod1.bool);

private

   type Object is
     new PortableServer.Servant_Base with record
      --  Insert components to hold the state
      --  of the implementation object.
      null;
   end record;

end mod1.Int1.Impl;
