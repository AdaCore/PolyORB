------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      P O L Y O R B . O B J E C T S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Utils;

package body PolyORB.Objects is

   use Ada.Streams;

   -----------------------
   -- Oid_To_Hex_String --
   -----------------------

   function Oid_To_Hex_String (Oid : Object_Id) return String is
   begin
      return Utils.SEA_To_Hex_String (Stream_Element_Array (Oid));
   end Oid_To_Hex_String;

   -----------------------
   -- Hex_String_To_Oid --
   -----------------------

   function Hex_String_To_Oid (S : String) return Object_Id is
   begin
      return Object_Id (Utils.Hex_String_To_SEA (S));
   end Hex_String_To_Oid;

   -------------------
   -- String_To_Oid --
   -------------------

   function String_To_Oid (S : String) return Object_Id is
      A : Object_Id (Stream_Element_Offset (S'First)
                  .. Stream_Element_Offset (S'Last));
      for A'Address use S'Address;
      pragma Import (Ada, A);
   begin
      return A;
   end String_To_Oid;

   -----------
   -- Image --
   -----------

   function Image (Oid : Object_Id) return String is
      Oid_S  : String (1 .. Oid'Length);
      for Oid_S'Address use Oid'Address;
      pragma Import (Ada, Oid_S);

      Result : String (1 .. Oid'Length) := Oid_S;
   begin
      for J in Result'Range loop
         if Character'Pos (Result (J)) not in 32 .. 127 then
            Result (J) := '.';
         end if;
      end loop;
      return Result;
   end Image;

end PolyORB.Objects;
