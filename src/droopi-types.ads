--  Base data types for the whole middleware.

--  $Id$

with Interfaces;

package Droopi.Types is

   pragma Pure;

   type    Short              is new Interfaces.Integer_16;
   type    Long               is new Interfaces.Integer_32;
   type    Long_Long          is new Interfaces.Integer_64;
   type    Unsigned_Short     is new Interfaces.Unsigned_16;
   type    Unsigned_Long      is new Interfaces.Unsigned_32;
   type    Unsigned_Long_Long is new Interfaces.Unsigned_64;
   type    Float              is new Interfaces.IEEE_Float_32;
   type    Double             is new Interfaces.IEEE_Float_64;
   type    Long_Double        is new Interfaces.IEEE_Extended_Float;
   subtype Char               is Standard.Character;
   subtype Wchar              is Standard.Wide_Character;
   type    Octet              is new Interfaces.Unsigned_8;
   subtype Boolean            is Standard.Boolean;

   --  type    String         is
   --    new Ada.Strings.Unbounded.Unbounded_String;
   --  type    Wide_String    is
   --    new Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

end Droopi.Types;
