-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
-------------------------------------------------
pragma Style_Checks (Off);

with CORBA;
with PortableServer;

package Inter1.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   function get_Attr1
     (Self : access Object)
     return CORBA.Float;

   procedure set_Attr1
     (Self : access Object;
      To : in CORBA.Float);

   function get_Attr2
     (Self : access Object)
     return CORBA.Boolean;

   procedure set_Attr2
     (Self : access Object;
      To : in CORBA.Boolean);

   function get_Attr3
     (Self : access Object)
     return CORBA.Long;

   function get_Attr4
     (Self : access Object)
     return CORBA.Long_Long;

   procedure set_Attr4
     (Self : access Object;
      To : in CORBA.Long_Long);

   function Name
     (Self : access Object;
      code : in CORBA.Short)
     return CORBA.String;

   procedure SName
     (Self : access Object;
      code : in CORBA.Short;
      str : in CORBA.String);

private

   type Object is
     new PortableServer.Servant_Base with record
      --  Insert components to hold the state
      --  of the implementation object.
      null;
   end record;

end Inter1.Impl;
