--  Base data types for the whole middleware.

--  $Id$

with Interfaces;

with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Unbounded;
with Ada.Unchecked_Deallocation;

package Droopi.Types is

   pragma Preelaborate;

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

   type    String         is
     new Ada.Strings.Unbounded.Unbounded_String;
   type    Wide_String    is
     new Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   type    Short_Ptr              is access all Short;
   type    Long_Ptr               is access all Long;
   type    Long_Long_Ptr          is access all Long_Long;
   type    Unsigned_Short_Ptr     is access all Unsigned_Short;
   type    Unsigned_Long_Ptr      is access all Unsigned_Long;
   type    Unsigned_Long_Long_Ptr is access all Unsigned_Long_Long;
   type    Float_Ptr              is access all Float;
   type    Double_Ptr             is access all Double;
   type    Long_Double_Ptr        is access all Long_Double;
   type    Char_Ptr               is access all Char;
   type    Wchar_Ptr              is access all Wchar;
   type    Octet_Ptr              is access all Octet;
   type    Boolean_Ptr            is access all Boolean;
   type    String_Ptr             is access all String;
   type    Wide_String_Ptr        is access all Wide_String;

   --  and the deallocation method for each pointer type

   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Short, Short_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Long, Long_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Long_Long, Long_Long_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Unsigned_Short, Unsigned_Short_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Unsigned_Long, Unsigned_Long_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Unsigned_Long_Long, Unsigned_Long_Long_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Float, Float_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Double, Double_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Long_Double, Long_Double_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Char, Char_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Wchar, Wchar_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Octet, Octet_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Boolean, Boolean_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (String, String_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Wide_String, Wide_String_Ptr);

   ---------------------------------
   -- String conversion functions --
   ---------------------------------

   function To_Droopi_String
     (Source : Standard.String)
     return String;

   function To_Standard_String
     (Source : String)
     return Standard.String;

   function To_Droopi_Wide_String
     (Source : Standard.Wide_String)
     return Wide_String;

   function To_Standard_Wide_String
     (Source : Wide_String)
     return Standard.Wide_String;

   type Identifier is new Droopi.Types.String;
   type RepositoryId is new Droopi.Types.String;
   type ScopedName is new Droopi.Types.String;

private

   pragma Inline (To_Droopi_String);
   pragma Inline (To_Standard_String);
   pragma Inline (To_Droopi_Wide_String);
   pragma Inline (To_Standard_Wide_String);

end Droopi.Types;
