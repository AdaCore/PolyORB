-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
-------------------------------------------------
pragma Style_Checks (Off);

with CORBA;
with tin_IDL_File;
with m1.int1.Skel;
pragma Elaborate (m1.int1.Skel);
pragma Warnings (Off, m1.int1.Skel);

package body m1.int1.Impl is


   function get_attr1
     (Self : access Object)
     return tin_IDL_File.New_Float
   is
      Result : tin_IDL_File.New_Float;
   begin

      --  Insert implementation of get_attr1

      return Result;
   end get_attr1;


   procedure set_attr1
     (Self : access Object;
      To : in tin_IDL_File.New_Float) is
   begin

      --  Insert implementation of set_attr1

      null;
   end set_attr1;


   function get_bool1
     (Self : access Object)
     return CORBA.Boolean
   is
      Result : CORBA.Boolean;
   begin

      --  Insert implementation of get_bool1

      return Result;
   end get_bool1;

end m1.int1.Impl;
