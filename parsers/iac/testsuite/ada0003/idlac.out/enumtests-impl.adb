-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
-------------------------------------------------
pragma Style_Checks (Off);

with EnumTests.Skel;
pragma Elaborate (EnumTests.Skel);
pragma Warnings (Off, EnumTests.Skel);

package body EnumTests.Impl is


   function get_attr_enum
     (Self : access Object)
     return EnumTests.Color
   is
      Result : EnumTests.Color;
   begin

      --  Insert implementation of get_attr_enum

      return Result;
   end get_attr_enum;


   procedure set_attr_enum
     (Self : access Object;
      To : in EnumTests.Color) is
   begin

      --  Insert implementation of set_attr_enum

      null;
   end set_attr_enum;


   procedure modif_enum
     (Self : access Object;
      C : in out EnumTests.Color;
      Returns : out EnumTests.Color) is
   begin

      --  Insert implementation of modif_enum

      null;
   end modif_enum;

end EnumTests.Impl;
