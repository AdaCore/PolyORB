-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
-------------------------------------------------
pragma Style_Checks (Off);

with mod1.Int1.Skel;
pragma Elaborate (mod1.Int1.Skel);
pragma Warnings (Off, mod1.Int1.Skel);

package body mod1.Int1.Impl is


   function get_Real_Number
     (Self : access Object)
     return mod1.Int1.New_Float
   is
      Result : mod1.Int1.New_Float;
   begin

      --  Insert implementation of get_Real_Number

      return Result;
   end get_Real_Number;


   procedure set_Real_Number
     (Self : access Object;
      To : in mod1.Int1.New_Float) is
   begin

      --  Insert implementation of set_Real_Number

      null;
   end set_Real_Number;


   function get_couleur
     (Self : access Object)
     return mod1.Int1.Color
   is
      Result : mod1.Int1.Color;
   begin

      --  Insert implementation of get_couleur

      return Result;
   end get_couleur;


   procedure set_couleur
     (Self : access Object;
      To : in mod1.Int1.Color) is
   begin

      --  Insert implementation of set_couleur

      null;
   end set_couleur;


   function get_b1
     (Self : access Object)
     return mod1.bool
   is
      Result : mod1.bool;
   begin

      --  Insert implementation of get_b1

      return Result;
   end get_b1;


   procedure set_b1
     (Self : access Object;
      To : in mod1.bool) is
   begin

      --  Insert implementation of set_b1

      null;
   end set_b1;

end mod1.Int1.Impl;
