-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
-------------------------------------------------
pragma Style_Checks (Off);

with CORBA;
with Inter1.Skel;
pragma Elaborate (Inter1.Skel);
pragma Warnings (Off, Inter1.Skel);

package body Inter1.Impl is


   function get_Attr1
     (Self : access Object)
     return CORBA.Float
   is
      Result : CORBA.Float;
   begin

      --  Insert implementation of get_Attr1

      return Result;
   end get_Attr1;


   procedure set_Attr1
     (Self : access Object;
      To : in CORBA.Float) is
   begin

      --  Insert implementation of set_Attr1

      null;
   end set_Attr1;


   function get_Attr2
     (Self : access Object)
     return CORBA.Boolean
   is
      Result : CORBA.Boolean;
   begin

      --  Insert implementation of get_Attr2

      return Result;
   end get_Attr2;


   procedure set_Attr2
     (Self : access Object;
      To : in CORBA.Boolean) is
   begin

      --  Insert implementation of set_Attr2

      null;
   end set_Attr2;


   function get_Attr3
     (Self : access Object)
     return CORBA.Long
   is
      Result : CORBA.Long;
   begin

      --  Insert implementation of get_Attr3

      return Result;
   end get_Attr3;


   function get_Attr4
     (Self : access Object)
     return CORBA.Long_Long
   is
      Result : CORBA.Long_Long;
   begin

      --  Insert implementation of get_Attr4

      return Result;
   end get_Attr4;


   procedure set_Attr4
     (Self : access Object;
      To : in CORBA.Long_Long) is
   begin

      --  Insert implementation of set_Attr4

      null;
   end set_Attr4;


   function Name
     (Self : access Object;
      code : in CORBA.Short)
     return CORBA.String
   is
      Result : CORBA.String;
   begin

      --  Insert implementation of Name

      return Result;
   end Name;


   procedure SName
     (Self : access Object;
      code : in CORBA.Short;
      str : in CORBA.String) is
   begin

      --  Insert implementation of SName

      null;
   end SName;

end Inter1.Impl;
