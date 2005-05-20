-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.adacore.com/polyorb/)
-------------------------------------------------
pragma Style_Checks (Off);

with m1.i1;
with m1.i2.Skel;
pragma Elaborate (m1.i2.Skel);
pragma Warnings (Off, m1.i2.Skel);

package body m1.i2.Impl is


   function get_attr1
     (Self : access Object)
     return m1.i1.t1
   is
      Result : m1.i1.t1;
   begin

      --  Insert implementation of get_attr1

      return Result;
   end get_attr1;


   procedure set_attr1
     (Self : access Object;
      To : in m1.i1.t1) is
   begin

      --  Insert implementation of set_attr1

      null;
   end set_attr1;


   function get_bool
     (Self : access Object)
     return m1.b1
   is
      Result : m1.b1;
   begin

      --  Insert implementation of get_bool

      return Result;
   end get_bool;


   procedure set_bool
     (Self : access Object;
      To : in m1.b1) is
   begin

      --  Insert implementation of set_bool

      null;
   end set_bool;

end m1.i2.Impl;
