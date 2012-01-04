------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      P O L Y O R B . O B J E C T S                       --
--                                                                          --
--                                 S p e c                                  --
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

--  Object identifier type. An Object_Id is an opaque data container
--  identifying one concrete object whithin a specific namespace.

with Ada.Streams;
with Ada.Unchecked_Deallocation;

package PolyORB.Objects is

   pragma Preelaborate;

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
