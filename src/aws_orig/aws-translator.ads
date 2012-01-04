------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       A W S . T R A N S L A T O R                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2000-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;
with Ada.Strings.Unbounded;

package AWS.Translator is

   function Base64_Encode
     (Data : Ada.Streams.Stream_Element_Array)
      return String;
   --  Encode Data using the base64 algorithm

   function Base64_Encode (Data : String) return String;
   --  Same as above but takes a string as input

   function Base64_Decode
     (B64_Data : String)
      return Ada.Streams.Stream_Element_Array;
   --  Decode B64_Data using the base64 algorithm

   function QP_Decode
     (QP_Data : String)
      return String;
   --  Decode QP_Data using the Quoted Printable algorithm

   function To_String
     (Data : Ada.Streams.Stream_Element_Array)
      return String;
   pragma Inline (To_String);
   --  Convert a Stream_Element_Array to a string. Note that as this routine
   --  returns a String it should not be used with large array as this could
   --  break the stack size limit. Use the routine below for large array.

   function To_Unbounded_String
     (Data : Ada.Streams.Stream_Element_Array)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Convert a Stream_Element_Array to an Unbounded_String.

   function To_Stream_Element_Array
     (Data : String)
      return Ada.Streams.Stream_Element_Array;
   pragma Inline (To_Stream_Element_Array);
   --  Convert a String to a Stream_Element_Array.

end AWS.Translator;
