----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
----------------------------------------------

with TVTForward.second;
with CORBA;
with CORBA.Value;

package TVTForward.first.Value_Impl is

   type Object is new CORBA.Value.Impl_Base with record
      s : CORBA.String;
      att : TVTForward.second.Value_Ref;
   end record;

   type Object_Ptr is access all Object'Class;

   function get_s
     (Self : access Object)
     return CORBA.String;

   procedure set_s
     (Self : access Object;
      To : in CORBA.String);

   function get_att
     (Self : access Object)
     return TVTForward.second.Value_Ref;

   procedure set_att
     (Self : access Object;
      To : in TVTForward.second.Value_Ref);

   function create
     (l : in CORBA.Long;
      s : in CORBA.String)
      return Object_Ptr;

   procedure print
     (Self : access Object);

end TVTForward.first.Value_Impl;
