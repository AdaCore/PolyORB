----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
----------------------------------------------

with CORBA;
with CORBA.Value;

package TVTForward.second.Value_Impl is

   type Object is new CORBA.Value.Impl_Base with record
      att : TVTForward.first_Forward.Value_Ref;
      l : CORBA.Long;
   end record;

   type Object_Ptr is access all Object'Class;

   function get_att
     (Self : access Object)
     return TVTForward.first_Forward.Value_Ref;

   procedure set_att
     (Self : access Object;
      To : in TVTForward.first_Forward.Value_Ref);

   function get_l
     (Self : access Object)
     return CORBA.Long;

   procedure set_l
     (Self : access Object;
      To : in CORBA.Long);

end TVTForward.second.Value_Impl;
