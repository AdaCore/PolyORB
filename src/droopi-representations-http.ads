

with Ada.Streams;

package  DROOPI.Representations.HTTP is

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
