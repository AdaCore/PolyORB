-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
-------------------------------------------------
pragma Style_Checks (Off);

with CORBA;
with module.m11.I111.Skel;
pragma Elaborate (module.m11.I111.Skel);
pragma Warnings (Off, module.m11.I111.Skel);

package body module.m11.I111.Impl is


   function get_value
     (Self : access Object)
     return CORBA.Float
   is
      Result : CORBA.Float;
   begin

      --  Insert implementation of get_value

      return Result;
   end get_value;


   procedure set_value
     (Self : access Object;
      To : in CORBA.Float) is
   begin

      --  Insert implementation of set_value

      null;
   end set_value;

end module.m11.I111.Impl;
