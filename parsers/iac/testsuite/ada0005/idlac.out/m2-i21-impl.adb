-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
-------------------------------------------------
pragma Style_Checks (Off);

with CORBA;
with m2.I21.Skel;
pragma Elaborate (m2.I21.Skel);
pragma Warnings (Off, m2.I21.Skel);

package body m2.I21.Impl is


   function is_greater
     (Self : access Object;
      f1 : in CORBA.Float;
      f2 : in CORBA.Float)
     return m2.I21.new_bool
   is
      Result : m2.I21.new_bool;
   begin

      --  Insert implementation of is_greater

      return Result;
   end is_greater;

end m2.I21.Impl;
