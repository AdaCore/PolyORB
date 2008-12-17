------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      P O L Y O R B . O B J E C T S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
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
   begin
      return A;
   end String_To_Oid;

   -----------
   -- Image --
   -----------

   function Image (Oid : Object_Id) return String is
      Oid_S  : String (1 .. Oid'Length);
      for Oid_S'Address use Oid'Address;

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
