with Ada.Streams; use Ada.Streams;
--  with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

--  with PolyORB.Types; use PolyORB.Types;
--  with PolyORB.Sockets;
--  with PolyORB.Buffers; use PolyORB.Buffers;
--  with PolyORB.Transport; use PolyORB.Transport;

--  with PolyORB.Utils.HTTP_Messages;

with Ada.Streams;


package PolyORB.Utils.HTTP is


   ---  Utilities functions

   function Base64_Encode (Data : Stream_Element_Array)
                          return String;

   function Base64_Encode (Data : in String) return String;

   function Base64_Decode (B64_Data : in String)
                          return Stream_Element_Array;


end  PolyORB.Utils.HTTP;
