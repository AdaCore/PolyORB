--  A data representation used for implementing the HTTP protocol.
--  HTTP is standardised by IETF RFC2616:
--  Hypertext Transfer Protocol -- HTTP/1.1.
--  R. Fielding, J. Gettys, J. Mogul, H. Frystyk, L. Masinter,
--  P. Leach, T. Berners-Lee. June 1999.

--  $Id$

with Ada.Streams;

package  DROOPI.Representations.HTTP is

   pragma Elaborate_Body;

   function Decode_URL (Str : in String) return String;
   --  The translations are:
   --     +     should be changed to a space
   --     %xy   should be replaced by the character whose code is xy

   function Encode_Stream (Data : Ada.Streams.Stream_Element_Array)  return String;
   --  Encode Data using the base64 algorithm

   function Encode_String (Data : in String) return String;
   --  Encode Data using the base64 algorithm also but it takes a string as input

   function Decode (B64_Data : in String) return Ada.Streams.Stream_Element_Array;
   --  Decode Data using the base64 algorithm

end  DROOPI.Representations.HTTP;
