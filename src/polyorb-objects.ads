------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      P O L Y O R B . O B J E C T S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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

--  Object identifier type. An Object_Id is an opaque data container
--  identifying one concrete object whithin a specific namespace.

with Ada.Streams;
with Ada.Unchecked_Deallocation;

package PolyORB.Objects is

   pragma Elaborate_Body;

   type Object_Id is new Ada.Streams.Stream_Element_Array;

   type Object_Id_Access is access all Object_Id;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object_Id, Object_Id_Access);

   function Oid_To_Hex_String (Oid : Object_Id) return String;
   pragma Inline (Oid_To_Hex_String);
   --  Convert an OID to a printable hex string representation

   function Hex_String_To_Oid (S : String) return Object_Id;
   pragma Inline (Hex_String_To_Oid);
   --  Convert an OID from a printable hex string representation

   function String_To_Oid (S : String) return Object_Id;
   pragma Inline (String_To_Oid);
   --  Convert an OID from a string

   function Image (Oid : Object_Id) return String;
   --  For debugging purposes

end PolyORB.Objects;
