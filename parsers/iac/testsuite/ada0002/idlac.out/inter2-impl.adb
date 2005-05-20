-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.adacore.com/polyorb/)
-------------------------------------------------
pragma Style_Checks (Off);

with CORBA;
with Inter2.Skel;
pragma Elaborate (Inter2.Skel);
pragma Warnings (Off, Inter2.Skel);

package body Inter2.Impl is


   function get_attr1
     (Self : access Object)
     return Inter2.New_Float
   is
      Result : Inter2.New_Float;
   begin

      --  Insert implementation of get_attr1

      return Result;
   end get_attr1;


   procedure set_attr1
     (Self : access Object;
      To : in Inter2.New_Float) is
   begin

      --  Insert implementation of set_attr1

      null;
   end set_attr1;


   function ConvertNew
     (Self : access Object;
      N : in CORBA.Float)
     return Inter2.New_Float
   is
      Result : Inter2.New_Float;
   begin

      --  Insert implementation of ConvertNew

      return Result;
   end ConvertNew;

end Inter2.Impl;
