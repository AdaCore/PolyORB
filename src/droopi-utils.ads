with Ada.Streams; use Ada.Streams;

package Droopi.Utils is

   function To_String (A : Stream_Element_Array) return String;
   --  Return a string of hex digits representing the contents of A.

   function To_Stream_Element_Array (S : String) return Stream_Element_Array;
   --  Return the Stream_Element_Array represented by the string of
   --  hex digits contaned in S.

end Droopi.Utils;
