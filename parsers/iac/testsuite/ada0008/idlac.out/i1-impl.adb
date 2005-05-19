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


   function get_str
     (Self : access Object)
     return CORBA.String
   is
      Result : CORBA.String;
   begin

      --  Insert implementation of get_str

      return Result;
   end get_str;


   procedure set_str
     (Self : access Object;
      To : in CORBA.String) is
   begin

      --  Insert implementation of set_str

      null;
   end set_str;


   function get_S
     (Self : access Object)
     return i1.new_string
   is
      Result : i1.new_string;
   begin

      --  Insert implementation of get_S

      return Result;
   end get_S;


   procedure set_S
     (Self : access Object;
      To : in i1.new_string) is
   begin

      --  Insert implementation of set_S

      null;
   end set_S;


   procedure min
     (Self : access Object;
      f1 : in i1.New_Float) is
   begin

      --  Insert implementation of min

      null;
   end min;


   procedure Add
     (Self : access Object;
      f1 : in out i1.New_Float;
      f2 : in CORBA.Float;
      Returns : out CORBA.Float) is
   begin

      --  Insert implementation of Add

      null;
   end Add;


   procedure minus
     (Self : access Object;
      f1 : in CORBA.Float;
      f2 : in CORBA.Float;
      r : out CORBA.Float;
      Returns : out i1.New_Float) is
   begin

      --  Insert implementation of minus

      null;
   end minus;

end i1.Impl;
