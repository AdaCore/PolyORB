------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       P O L Y O R B . O P A Q U E                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2004 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  Utility declarations for low-level memory management.

with Ada.Streams;
with Ada.Unchecked_Deallocation;
with System;

package PolyORB.Opaque is

   pragma Preelaborate;

   ----------------------------------------
   -- All-purpose memory allocation type --
   ----------------------------------------

   type Zone_Access is access all Ada.Streams.Stream_Element_Array;
   --  A storage zone: an array of bytes.

   procedure Free is new Ada.Unchecked_Deallocation
     (Ada.Streams.Stream_Element_Array, Zone_Access);

   --------------------------------------
   -- All-purpose memory location type --
   --------------------------------------

   subtype Opaque_Pointer is System.Address;

   function Is_Null (P : Opaque_Pointer) return Boolean;
   pragma Inline (Is_Null);

   procedure Fill
     (P      : Opaque_Pointer;
      Len    : Ada.Streams.Stream_Element_Count;
      Filler : Ada.Streams.Stream_Element := 16#aa#);
   pragma Inline (Fill);
   --  Fill Len elements starting at P with the specified filler value
   --  (used for debugging purposes).

end PolyORB.Opaque;
