--  Miscellaneous utility subprograms.

--  $Id$

with Ada.Streams;

package PolyORB.Utils is

   pragma Pure;

   function To_String
     (A : Ada.Streams.Stream_Element_Array)
     return String;
   --  Return a string of hexadecimal digits representing the
   --  contents of A.

   function To_Stream_Element_Array
     (S : String)
     return Ada.Streams.Stream_Element_Array;
   --  Return the Stream_Element_Array represented by the
   --  string of hexadecimal digits contaned in S.

end PolyORB.Utils;
