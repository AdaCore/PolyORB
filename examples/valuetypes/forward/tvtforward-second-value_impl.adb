----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA;

package body TVTForward.second.Value_Impl is


   function get_att
     (Self : access Object)
     return TVTForward.first_Forward.Value_Ref is
   begin
      return Self.all.att;
   end get_att;


   procedure set_att
     (Self : access Object;
      To : in TVTForward.first_Forward.Value_Ref) is
   begin
      Self.all.att := To;
   end set_att;


   function get_l
     (Self : access Object)
     return CORBA.Long is
   begin
      return Self.all.l;
   end get_l;


   procedure set_l
     (Self : access Object;
      To : in CORBA.Long) is
   begin
      Self.all.l := To;
   end set_l;

end TVTForward.second.Value_Impl;
