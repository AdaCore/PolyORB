-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.adacore.com/polyorb/)
-------------------------------------------------
pragma Style_Checks (Off);

with CORBA;
with i1.Skel;
pragma Elaborate (i1.Skel);
pragma Warnings (Off, i1.Skel);

package body i1.Impl is


   function get_val1
     (Self : access Object)
     return CORBA.Float
   is
      Result : CORBA.Float;
   begin

      --  Insert implementation of get_val1

      return Result;
   end get_val1;


   procedure set_val1
     (Self : access Object;
      To : in CORBA.Float) is
   begin

      --  Insert implementation of set_val1

      null;
   end set_val1;


   function get_val2
     (Self : access Object)
     return CORBA.Float
   is
      Result : CORBA.Float;
   begin

      --  Insert implementation of get_val2

      return Result;
   end get_val2;


   procedure set_val2
     (Self : access Object;
      To : in CORBA.Float) is
   begin

      --  Insert implementation of set_val2

      null;
   end set_val2;


   function get_tab_val
     (Self : access Object)
     return CORBA.Float
   is
      Result : CORBA.Float;
   begin

      --  Insert implementation of get_tab_val

      return Result;
   end get_tab_val;


   procedure set_tab_val
     (Self : access Object;
      To : in CORBA.Float) is
   begin

      --  Insert implementation of set_tab_val

      null;
   end set_tab_val;


   function get_tab
     (Self : access Object)
     return i1.Tab_Float
   is
      Result : i1.Tab_Float;
   begin

      --  Insert implementation of get_tab

      return Result;
   end get_tab;


   procedure set_tab
     (Self : access Object;
      To : in i1.Tab_Float) is
   begin

      --  Insert implementation of set_tab

      null;
   end set_tab;

end i1.Impl;
