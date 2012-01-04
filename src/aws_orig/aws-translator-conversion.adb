------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           C O N V E R S I O N                            --
--                                                                          --
--                                 B o d y                                  --
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

--  $RCSfile: aws-translator-conversion-f.adb,v $
--  $LastChangedRevision$
--  $LastChangedDate$
--  $LastChangedBy$

--  Fast convertion between String and Stream_Element_Array.
--  Only for Ada compilers and platforms, where it is possible.

with Ada.Unchecked_Conversion;

separate (AWS.Translator)

package body Conversion is

   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------

   function To_Stream_Element_Array
     (Data : String)
     return Stream_Element_Array
   is

      subtype Fixed_String is String (Data'First .. Data'Last);

      subtype Fixed_Array is Stream_Element_Array
        (Stream_Element_Offset (Data'First)
         .. Stream_Element_Offset (Data'Last));

      function To_Stream_Elements is
        new Ada.Unchecked_Conversion (Fixed_String, Fixed_Array);

   begin
      return To_Stream_Elements (Data);
   end To_Stream_Element_Array;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Data : Stream_Element_Array)
     return String
   is

      subtype Fixed_String is String (Integer (Data'First)
        .. Integer (Data'Last));

      subtype Fixed_Array is Stream_Element_Array
         (Data'First .. Data'Last);

      function To_Characters is
        new Ada.Unchecked_Conversion (Fixed_Array, Fixed_String);

   begin
      return To_Characters (Data);
   end To_String;

end Conversion;
