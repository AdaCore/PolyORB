package Charset is

   function Is_Alphabetic_Character (C : Character) return Boolean;
   --  Alphabetic characters of ISO Latin-1

   function Is_Identifier_Character (C : Character) return Boolean;
   --  Alphabetic character or digit or underscore character

   procedure To_Lower (S : in out String);
   function To_Lower (S : String) return String;
   function To_Lower (C : Character) return Character;
   --  Translate into lower case form

end Charset;
