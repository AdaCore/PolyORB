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

   function get_str
     (Self : access Object)
     return CORBA.String;

   procedure set_str
     (Self : access Object;
      To : in CORBA.String);

   function get_S
     (Self : access Object)
     return i1.new_string;

   procedure set_S
     (Self : access Object;
      To : in i1.new_string);

   procedure min
     (Self : access Object;
      f1 : in i1.New_Float);

   procedure Add
     (Self : access Object;
      f1 : in out i1.New_Float;
      f2 : in CORBA.Float;
      Returns : out CORBA.Float);

   procedure minus
     (Self : access Object;
      f1 : in CORBA.Float;
      f2 : in CORBA.Float;
      r : out CORBA.Float;
      Returns : out i1.New_Float);

private

   type Object is
     new PortableServer.Servant_Base with record
      --  Insert components to hold the state
      --  of the implementation object.
      null;
   end record;

end i1.Impl;
