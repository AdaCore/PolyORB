-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
-------------------------------------------------
pragma Style_Checks (Off);

with CORBA;
with tin_IDL_File;
with PortableServer;

package m1.int1.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   function get_attr1
     (Self : access Object)
     return tin_IDL_File.New_Float;

   procedure set_attr1
     (Self : access Object;
      To : in tin_IDL_File.New_Float);

   function get_bool1
     (Self : access Object)
     return CORBA.Boolean;

private

   type Object is
     new PortableServer.Servant_Base with record
      --  Insert components to hold the state
      --  of the implementation object.
      null;
   end record;

end m1.int1.Impl;
